#!/usr/bin/env python

import os, sys
import argparse
import glob
import socket
import tempfile

from threading import Thread

from mfixgui.version import __version__

try:
    import mfixsolver as M
except ImportError as e:
    print("Cannot import solver module", e)
    raise

port = 0 # OS finds free port

job_id = (os.environ.get('JOB_ID') # GridEngine
          or os.environ.get('PBS_JOBID')) # Torque

run_cmd = os.environ.get('MFIX_RUN_CMD', ' '.join(sys.argv))

job_is_remote = (job_id is not None) or ('qsub' in run_cmd)

run_name = None # gets set when *.mfix file is loaded

class MfixThread(Thread):
    def __init__(self, filename):
        Thread.__init__(self)
        self.filename = filename

    def run(self):
        return M.main.run_mfix0(bytes(self.filename, encoding="utf-8"))


def parse_args():
    parser = argparse.ArgumentParser(description='Welcome to PYMFiX')
    ARG = parser.add_argument
    ARG('-f', '--file', default='mfix.dat',
        help='specify an input file (*.mfx or *.dat)')
    ARG('-p', '--print-flags', action='store_true',
        help='display the solver configuration flags')
    ARG('-c', '--clean', action='store_true',
        help='clean output files before starting run')
    ARG('-d', '--daemon', action='store_true',
        help='run detached with stdout/stderr redirection')
    ARG('-s', '--server', action='store_true',
        help='start HTTP server')
    ARG('-v', '--version', action='version', version=__version__)
    return parser.parse_args()


def clean():
    for ext in 'vt?', 'sp?',  'out', 'res', 'pvd', 'res', 'log', 'pid', 'env':
        for ext in ext, ext.upper():
            for f in glob.glob('*.' + ext):
                print("removing %s" % f)
                os.unlink(f)
    for f in glob.glob('fort.*'):
        os.unlink(f)

def get_run_name(filename):
    global run_name
    for line in open(filename, encoding='utf-8', errors='replace'):
        line = line.strip()
        if line.lower().startswith('run_name') and '=' in line:
            tok = line.split('=', 1)[1].strip()
            # Remove quotes if present.
            # NB: Due to a bug, in some files, run_name may lack quotes
            if tok.startswith('"') or tok.startswith("'"):
                tok = tok[1:]
            if tok.endswith('"') or tok.endswith("'"):
                tok = tok[:-1]
            run_name = tok.strip()
            break


ssl_enabled = False
def setup_ssl():
    global ssl_enabled
    ssl_enabled = False


def write_pidfile():
    # Avoid importing Flask in global namespace, to keep -p etc fast
    from mfixgui.pymfix_webserver import secret_key
    pid = os.getpid()
    protocol = 'https' if ssl_enabled else 'http'
    if job_is_remote:
        hostname = socket.gethostname()
    else:
        # issues/407 just use loopback for local jobs
        hostname = '127.0.0.1'
    with open(run_name + '.pid', 'w', encoding='utf-8', errors='replace') as f:
        f.write('host=%s\n' % hostname)
        f.write('pid=%s\n' % pid)
        f.write('url=%s://%s:%s\n' %
                (protocol, hostname, port))
        f.write('token=x-pymfix-auth:%s\n' % secret_key)
        f.write('job_id=%s\n' % job_id)
        f.write('run_cmd=%s\n' % run_cmd)
    # Save environment for debugging
    with open(run_name+'.env', 'w', encoding='utf-8', errors='replace') as f:
        for (k,v) in sorted(os.environ.items()):
            f.write('%s=%s\n' % (k,v))


def find_free_port():
    # Note that this is racy - the port could get used by
    # another program before the webserver binds to it
    global port
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(("", 0))
    port = sock.getsockname()[1]
    sock.close()


def redirect_stdio():
    # Send stdout/stderr to tempfiles so we can collect them and send to
    # client via HTTP
    pid = os.getpid()
    stdout_file = tempfile.TemporaryFile(prefix='mfix-%s-out-' % pid, buffering=0)
    stderr_file = tempfile.TemporaryFile(prefix='mfix-%s-err-' % pid, buffering=0)
    os.dup2(stdout_file.fileno(), sys.stdout.fileno())
    os.dup2(stderr_file.fileno(), sys.stderr.fileno())


def start_webserver():
    # Avoid importing pymfix_webserver in global namespace,
    # because we don't always need it and loading Flask is slow
    import mfixgui.pymfix_webserver as W
    # Set globals in W
    W.M = M
    W.mfix_thread = mfix_thread
    W.run_name = run_name
    W.options = options
    W.start(port=port)


def main():
    global mfix_thread, options
    options = parse_args()

    if options.print_flags:
        M.main.print_flags()
        sys.exit(0)

    if job_is_remote:
        # Enable daemon mode so we can collect remote stderr/stdout/status
        # However, this should really be done by invoking pymfix with '-d' option
        options.daemon = True
        # Should we enable this all the time?  It allows reconnect
        # what is the downside?

    dirname, filename = os.path.split(options.file)
    if dirname:
        os.chdir(dirname)

    if options.clean:
        clean()

    # set the global run_name variable
    # We do this here instead of getting it from the solver,
    #  because the solver may fail to load the input file
    get_run_name(filename)
    if not run_name:
        print("ERROR, run_name not set in %s" % filename)
        sys.exit(1)

    # TODO check if pidfile exists & process is running.  If so, refuse
    #  to start

    M.parallel_mpi.parallel_init()

    if options.server:
        mfix_thread = MfixThread(filename)
        mfix_thread.setDaemon(True) #Note, this is not controlled by -d flag, mfix_thread is always daemonic

        mfix_thread.start()

        # start the Flask server on rank 0
        if (M.compar.mype == 0):
            if options.daemon:
                redirect_stdio() # Should we do this on all nodes?
            #setup_ssl() #Does nothing at present
            find_free_port()
            write_pidfile()
            start_webserver()
            try:
                os.unlink(run_name + '.pid')
            except:
                pass
        else:
            mfix_thread.join()

    else: # --server not specified, don't start webserver
        # don't need separate thread for mfix
        M.main.run_mfix0(filename)

    M.parallel_mpi.parallel_fin()


if __name__ == '__main__':
    main()
