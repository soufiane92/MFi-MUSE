#!/usr/bin/env python3

"""Script for deploying MFIX conda packages and documentation to
mfix.netl.doe.gov

 This could be done by a gitlab-runner job, but we don't have gitlab-runner on
 mfix.netl.doe.gov

"""

import argparse
import glob
import os
import shutil
import subprocess
import sys
import tarfile
import tempfile
import zipfile

from grp import getgrall
from pwd import getpwall

import gitlab

# override for testing
HTTP_HOME = os.path.join(os.sep, "var", "www", "mfix", "html")

GROUP = "apache" # may need to override
USER = "mfix" # may need to override

DEVELOP_BRANCH = 'develop'

GITLAB_URL = "https://mfix.netl.doe.gov/gitlab"
NOARCH_TARBALL_DOC_PATH = "share/mfix/doc/html"


def main():
    """ deploy conda packages, source tarball, and documentation """

    parser = argparse.ArgumentParser(description='Deploy MFiX on mfix.netl.doe.gov')
    parser.add_argument(
        '-t', '--tag', default=DEVELOP_BRANCH,
        help='Specify Git tag to be deployed')
    parser.add_argument(
        '-k', '--token',
        help='Get token at: https://mfix.netl.doe.gov/gitlab/profile/personal_access_tokens')
    parser.add_argument(
        '-c', '--conda', default='conda',
        help='Path to conda command')
    parser.add_argument(
        '-m', '--mode', default='devel', choices=['devel', 'dist'],
        help='Mode (directory name). Use devel for testing and dist for actual releases.')

    args = parser.parse_args()
    deploy_mode = args.mode
    tag = args.tag
    token = args.token
    conda = args.conda

    if not token:
        print("Need to set TOKEN.")
        sys.exit()

    if tag == DEVELOP_BRANCH and deploy_mode == 'dist':
        print("Refusing to deploy", DEVELOP_BRANCH, "to dist. Exiting.")
        sys.exit()

    print("Getting project info from Gitlab...")
    develop_mfix_id = 13
    develop_mfix = gitlab.Gitlab(GITLAB_URL,
                                 token,
                                 api_version=4).projects.get(develop_mfix_id)

    if tag != DEVELOP_BRANCH:
        _check_tag_exists(develop_mfix, tag)

    with tempfile.TemporaryDirectory() as tmpdir:
        conda_pkg = _deploy_conda_repo(develop_mfix, tmpdir, tag, deploy_mode, conda)
        _deploy_docs(conda_pkg, tmpdir, tag)
        if tag != DEVELOP_BRANCH:
            _deploy_source_tarball(develop_mfix, tmpdir, tag)

        if tag == DEVELOP_BRANCH:
            _deploy_code_coverage(develop_mfix, tag, deploy_mode)


def _check_tag_exists(develop_mfix, deploy_tag):
    if not deploy_tag:
        print("Need to set TAG.")
        sys.exit()

    tags = []
    for tag in develop_mfix.tags.list(as_list=False):
        if tag.name == deploy_tag:
            break
        tags.append(tag.name)
    else:
        print("Tag not found: ", deploy_tag)
        print("Project tags found: ", ' '.join(tags))
        sys.exit()


def _deploy_conda_repo(develop_mfix, tmpdir, tag, deploy_mode, conda):
    print("Deploying conda packages...")

    repo_home = os.path.join(HTTP_HOME, deploy_mode)
    if deploy_mode != 'dist' and os.path.exists(repo_home):
        for filename in os.listdir(repo_home):
            if filename.startswith('mfix'):
                os.remove(os.path.join(repo_home, filename))
    if not os.path.exists(repo_home):
        os.makedirs(repo_home)

    _extract_artifacts(develop_mfix, tmpdir, tag, 'build:mfix-metapackage')

    platforms = [
        "linux-64",
        "noarch",
        "osx-64",
        "win-64",
    ]
    for platform in platforms:
        print("Deploying for:", platform)
        for conda_pkg in glob.glob(os.path.join(tmpdir, "dist", platform, '*.tar.bz2')):
            if tag != DEVELOP_BRANCH and tag not in conda_pkg:
                continue
            dest = os.path.join(repo_home, platform)
            if not os.path.exists(dest):
                os.makedirs(dest)
            shutil.copy(conda_pkg, dest)
            subprocess.check_call([conda, "index", dest])
            if 'mfix-doc' in conda_pkg:
                doc_pkg = conda_pkg


    _do_chown(os.path.join(HTTP_HOME, deploy_mode))
    print("Deployed conda packages.")
    print()

    env_ymls = ['linux64', 'osx64', 'win64']
    for platform in env_ymls:
        shutil.copy(os.path.join(tmpdir, 'build-aux', 'environment-' + platform + '.yml'),
                    os.path.join(HTTP_HOME, 'yml', 'env-mfix-18.1.0-' + platform + '.yml'))

    return doc_pkg # noarch/mfix-doc*.bz2

def _extract_artifacts(develop_mfix, destdir, tag, job_name):
    print("Getting artifacts for job {}...".format(job_name))

    job = None
    for j in develop_mfix.jobs.list(ref=tag, as_list=False):
        if j.ref == tag and j.name == job_name and j.status == 'success':
            job = j
            break

    class ArtifactStream(object):
        """ http://python-gitlab.readthedocs.io/en/stable/gl_objects/builds.html """
        def __init__(self, art):
            self._fd = open(art, 'wb')

        def __call__(self, chunk):
            self._fd.write(chunk)

    art = '{jobname}-artifacts.zip'.format(jobname=job.name)
    target = ArtifactStream(art)
    job.artifacts(streamed=True, action=target)
    del target  # flushes data to disk
    artifactzip = zipfile.ZipFile(art, 'r')
    artifactzip.extractall(destdir)
    artifactzip.close()


def _deploy_docs(conda_pkg, tmpdir, tag):
    doc_home = os.path.join(HTTP_HOME, "doc", "mfix", tag)
    if os.path.exists(doc_home):
        shutil.rmtree(doc_home)

    print("Extracting docs to", doc_home, " ...")
    with tarfile.open(conda_pkg, 'r:bz2') as tarball:
        docs = [doc for doc in tarball.getmembers() if NOARCH_TARBALL_DOC_PATH in doc.name]
        tarball.extractall(tmpdir, members=docs)

    full_docpath = os.path.join(tmpdir, NOARCH_TARBALL_DOC_PATH)
    shutil.move(full_docpath, doc_home)

    _do_chown(doc_home)
    print("Deployed docs.")
    print()


def _deploy_source_tarball(develop_mfix, tmpdir, tag):
    print("Downloading source tarball...")

    src_tarball = "mfix-{TAG}.tar".format(TAG=tag)
    with open(src_tarball, 'wb') as tarball:
        tarball.write(develop_mfix.repository_archive(sha=tag))

    with tarfile.open(src_tarball) as tar:
        tar.extractall(tmpdir)

    print("Renaming SHA and adding .tarball-version...")
    globbed = glob.glob(os.path.join(tmpdir, "mfix-{TAG}-*".format(TAG=tag)))
    assert len(globbed) == 1
    tarball_dirname = "mfix-{TAG}".format(TAG=tag)
    tarball_dirpath = os.path.join(tmpdir, tarball_dirname)
    os.rename(globbed[0], tarball_dirpath)
    with open(os.path.join(tarball_dirname, ".tarball-version"), "w") as versionfile:
        versionfile.write(tag)

    src_tarball = "mfix-{TAG}.tar.gz".format(TAG=tag)
    with tarfile.open(src_tarball, "w:gz") as tarball:
        tarball.add(tarball_dirpath, arcname=tarball_dirname)

    src_dir = os.path.join(HTTP_HOME, "source", "mfix")
    if not os.path.exists(src_dir):
        os.makedirs(src_dir)

    tarball_path = os.path.join(src_dir, os.path.basename(src_tarball))
    print("Moving tarball to ", tarball_path)
    if os.path.exists(tarball_path):
        os.remove(tarball_path)
    shutil.move(src_tarball, src_dir)
    _do_chown(src_dir)
    print("Deployed source tarball.")
    print()


def _deploy_code_coverage(develop_mfix, tag, deploy_mode):
    print("Downloading solver code coverage...")
    repo_home = os.path.join(HTTP_HOME, deploy_mode)
    _extract_artifacts(develop_mfix, repo_home, tag, 'ctest:coverage')
    _do_chown(os.path.join(HTTP_HOME, deploy_mode))
    print("Deployed code coverage.")
    print()


def _do_chown(top_path):
    if (USER in [p.pw_name for p in getpwall()] and GROUP in [p.pw_name for p in getgrall()]):
        for root, _, files in os.walk(top_path):
            for filename in files:
                shutil.chown(os.path.join(root, filename), GROUP, USER)


if __name__ == '__main__':
    main()
