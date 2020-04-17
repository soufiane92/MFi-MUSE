"""This file is will collect the .mfixguiinfo files data and save the info
to a single file for the gui. To use, run:

> make thumbnails

from the mfixgui directory"""

import json
import os
from mfixgui.tools import SCRIPT_DIRECTORY


def make_info_dict():
    info_dict = {}
    for path, _, files in os.walk('../'):
        # make sure we don't look in the build dir
        if 'build' not in path and '.mfixguiinfo' in files:
            name = os.path.basename(path)
            with open(os.path.join(path, '.mfixguiinfo'), encoding='utf-8', errors='replace') as f:
                info = f.readlines()[0].split(',')
                d = info_dict[name] = {}
                for k, v in zip(('solver', 'geometry', 'chemistry', 'description'), info):
                    d[k] = v
            os.remove(os.path.join(path, '.mfixguiinfo'))

    return info_dict


def get_template_info():
    info = {}
    filename = os.path.join(SCRIPT_DIRECTORY, 'tools', 'template_data.json')
    if os.path.exists(filename):
        with open(filename, encoding='utf-8', errors='replace') as f:
            info = json.load(f)
    return info


def main():
    info_dict = make_info_dict()
    with open('./tools/template_data.json', 'w') as f:
        json.dump(info_dict, f)


if __name__ == '__main__':
    main()
