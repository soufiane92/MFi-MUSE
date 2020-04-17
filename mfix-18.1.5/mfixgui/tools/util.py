# -*- coding: utf-8 -*-

"""utility functions which do not depend on Qt"""

import os
import copy
import re
import math
import operator
import subprocess
from collections import OrderedDict
from pdb import set_trace
from functools import reduce
from urllib.request import urljoin, pathname2url

def path2url(path):
    """Convert path to url."""
    return urljoin('file:', pathname2url(path))

def format_key_with_args(key, args=None):
    if args is not None and args != []:
        if isinstance(args, int):
            args = [args]
        return "%s(%s)" % (key, ','.join(str(a) for a in args))
    else:
        return str(key)


def parse_key_with_args(string):
    # inverse of format_key_with_args
    if string.endswith(')'):
        key, args = string[:-1].split('(')
        args = [int(arg) for arg in args.split(',')]
    else:
        key = string
        args = []
    return key, args


def plural(n, word):
    fmt = "%d %s" if n == 1 else "%d %ss"
    return fmt % (n, word)


def drop_row_column_triangular(a, n, r):
    # Inputs:
    #  a :  upper-triangular n by n matrix represented as a list (n)(n+1)/2
    #  n :  size of input
    #  r :  index of row/column to drop, 1-based
    # Return:
    #  list representing matrix "a" with row/column "r" removed
    ret = []
    i = j = 1
    for x in a:
        if i != r and j != r:
            ret.append(x)
        j += 1
        if j > n:
            i += 1
            j = i
    return ret


def append_row_column_triangular(a, n, fill_value=None):
    # Append a row and column to an upper-triangular rank-n matrix
    ret = []
    i = j = 1
    for x in a:
        ret.append(x)
        if j == n:
            ret.append(fill_value)
        j += 1
        if j > n:
            i += 1
            j = i
    ret.append(fill_value)
    return ret

def get_unique_string(base, listofstrings):
    "uniquify a string"
    if base in listofstrings:
        # look for number at end
        nums = re.findall(r'[\d]+$', base)
        if nums:
            number = int(nums[-1]) + 1
            base = base.replace(nums[-1], '')
        else:
            number = 1
        base = get_unique_string(base + str(number), listofstrings)

    return base


def num_to_time(time, unit='s', outunit='time', digits=0):
    """Convert time with a unit to another unit."""
    unit = unit.lower()
    time = float(time)

    if not (isinstance(time, (float, int)) and math.isfinite(time)):
        return '∞'
    # convert time to seconds
    if unit in ['d', 'days']:
        time *= 60 * 60 * 24
    elif unit in ['h', 'hr']:
        time *= 60 * 60
    elif unit in ['m', 'min']:
        time *= 60

    if outunit == 'time':
        d,h,m,s = reduce(
            lambda ll, b: divmod(ll[0], b) + ll[1:], [(time, ), 60, 60, 24])

        t = '{:02d}:{:02d}:{:02d}:{:02d}'.format(*(int(t) for t in (d,h,m,s)))
        if digits > 0:
            fraction = round(time - int(time), digits)
            t += ('%.*f' % (digits, fraction))[1:] # strip leading 0
        return t

    elif outunit in ['d', 'days']:
        return time / (60.0 * 60.0 * 24.0)

    elif outunit in ['h', 'hr', 'hrs']:
        return time / (60.0 * 60.0)

    elif outunit in ['m', 'min', 'mins']:
        return time / (60.0)

    else:
        return time


def topological_sort(dependency_dict):
    '''
    Sort the dependency tree.
    Inspired by: http://code.activestate.com/recipes/578272-topological-sort/
    '''

    data = copy.deepcopy(dependency_dict)

    # Ignore self dependencies.
    for k, v in data.items():
        v.discard(k)
    # Find all items that don't depend on anything.
    extra_items_in_deps = reduce(set.union, data.values()) - set(data.keys())
    # Add empty dependences where needed
    data.update({item: set() for item in extra_items_in_deps})
    while True:
        ordered = set(item for item, dep in data.items() if not dep)
        if not ordered:
            break
        yield ordered
        data = {item: (dep - ordered)
                for item, dep in data.items()
                if item not in ordered}
    assert not data, "Cyclic dependencies exist among these items:\n%s" % '\n'.join(repr(x) for x in data.items())


def sort_dict(dict_, key, start=0):
    """given an dict of dicts and a key, sort the outside dict based on the
    value of one of the the internal dict's keys and return the sorted
    OrderedDict"""
    sorted_enum = enumerate(
        sorted(
            [(k, v[key]) for k, v in dict_.items()],
            key=operator.itemgetter(1)), start)
    return OrderedDict([(k, dict_[old_k]) for k, (old_k, v) in sorted_enum])


def extract_config(path):
    '''Given a path to a file, extract the section that starts with ## CONFIG
    and ends with ## END CONFIG'''
    config = []
    script = []
    in_config = False
    with open(path, encoding='utf-8', errors='replace') as f:
        for line in f:
            l = line.rstrip()
            if '## CONFIG' in l:
                in_config = True
            elif '## END CONFIG' in l:
                in_config = False
            elif in_config:
                config.append(l)
            else:
                script.append(l)
    return '\n'.join(config), '\n'.join(script + [''])


def replace_with_dict(string, replacements):
    '''given a string and a dict, replace all dict.key found in the string
    with the corresponding dict.value'''

    for key, value in replacements.items():
        string = string.replace('${' + key + '}', str(value))
    return string


def is_vnc():
    """determine if the gui is running in vnc"""
    if os.name == 'nt':
        return False
    try:
        xdpyinfo = subprocess.Popen(
            'xdpyinfo', stdout=subprocess.PIPE).communicate()[0]
        return 'vnc' in str(xdpyinfo)
    except:
        return False

def get_username():
    """attempt to return the current user name"""
    name = 'unknown'
    for e in ['USER', 'user', 'USERNAME', 'username']:
        name = os.environ.get(e)
        if name:
            break
    if not name:
        name = 'unknown'
    return name


def convert_string_to_python(string):
    """Attempt to convert strings to python types"""

    if not string:
        return ''

    # remove all quotes
    string = string.replace("'", '').replace('"', '')
    # remove any leading or trailing space, after removing quotes
    string = string.strip()
    # Remove comma separators if present
    if string.endswith(','):
        string = string[:-1]

    # lower-case version of string
    s_low = string.lower()

    if s_low in ('.t.', '.true.'):
        return True
    elif s_low in ('.f.', '.false.'):
        return False

    # Maybe it's a number (would a regex be better?)
    if any(char.isdigit() for char in string):
        try:
            return int(string)
        except ValueError:
            try:
                return float(string)
            except ValueError:
                pass

    # default - return string unchanged
    return string


def safe_float(value, default=0.0):
    """try to convert the value to a float, if ValueError or None, return default"""
    if value is None:
        return default
    try:
        return float(value)
    except ValueError:
        return default


def safe_int(value, default=0):
    """try to convert the value to a int, if ValueError or None, return default"""
    if value is None:
        return default
    try:
        return int(value)
    except ValueError:
        return default

def case_insensitive(pattern):
    """create case insestive glob/fnmatch/re pattern for every letter"""
    def either(c):
        return '[%s%s]'%(c.lower(),c.upper()) if c.isalpha() else c
    return ''.join(map(either, pattern))

def rround(d, places=6):
    """Recursively round all floats encountered in data structure d"""
    if isinstance(d, float):
        return round(d, places)
    elif isinstance(d, dict):
        return dict((k, rround(v, places)) for (k,v) in d.items())
    elif isinstance(d, list):
        return list(rround(v) for v in d)
    elif isinstance(d, tuple):
        return tuple(rround(v) for v in d)
    else:
        return d

# Debugging hooks
def debug_trace():
    """Set a tracepoint in the Python debugger that works with Qt"""
    set_trace()


def main():
    def test_drop_add_triangular():
        a = [ 1, 2, 3, 4,
                 5, 6, 7,
                    8, 9,
                       10]
        b = drop_row_column_triangular(a, 4, 2)
        assert b == [1, 3, 4,
                        8, 9,
                           10]

        c = drop_row_column_triangular(b, 3, 3)
        assert c == [1, 3,
                        8]

        d = append_row_column_triangular(c, 2, 999)
        assert d == [1, 3, 999,
                        8, 999,
                           999]

    test_drop_add_triangular()

if __name__ == '__main__':
    main()
