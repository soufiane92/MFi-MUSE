#!/usr/bin/env python

import os
import subprocess
import time
from mfixgui.tools.keyword_args import keyword_args

thisdir = os.path.abspath(os.path.dirname(__file__))
TOP = os.path.abspath(thisdir + '/../../')

use_sqlite = False

if use_sqlite:
    import sqlite3
    conn = sqlite3.connect('keywords.db')
    curs = conn.cursor()
    curs.execute('''create table keywords (
                    keyword text primary key, gui bool, srs bool,
                    tut bool, tst bool, ltut bool, ltst bool)''')

all_keys = list(keyword_args.keys())

# These are supported in the GUI but are not literal
dynamic_keys = {a+b for a in ('bc_', 'ic_', 'is_', 'ps_', 'vtk_')
                for b in ('x_e', 'x_w', 'y_n', 'y_s', 'z_b', 'z_t')}

# Cyclic keys are dynamic
for pat in 'delp_%s', 'cyclic_%s', 'cyclic_%s_pd':
    dynamic_keys.update({pat % axis for axis in 'xyz'})


# Setters created in fluid_handler and solids_handler
dynamic_keys.update({'usr_%s' %s for s in ('cps', 'cpg',
                                           'difg',
                                           'kg', 'ks',
                                           'mus', 'mug',
                                           'rog')})


# Keys defined in .ui files (auto-registered widgets)
ui_keys = set()
for l in subprocess.getoutput('cd %s/mfixgui/uifiles; git grep _keyword_' % TOP).split():
    if l.startswith('name'):
        key = l.split('_keyword_', 1)[1].split('_args_',1)[0].split('"',1)[0]
        if key not in all_keys and key[-2]=='_' and key[-1].isdigit(): # Trim numeric suffix
            key = key[:-2]
        if key not in all_keys:
            raise ValueError("Unknown key %s", key)
        ui_keys.add(key)

def git_grep(loc, key):
    if key == 'c':
        return False
    if loc=='mfixgui': # Special handling
        if key in dynamic_keys or key in ui_keys or key in converted:
            return True
        lines = subprocess.getoutput('cd %s/%s; git grep -iw %s |\
            egrep -v "keyword_args.py|unit_conversion.py|tests/|uifiles/"'
                                     % (TOP, loc, key)).split('\n')
        for line in lines:
            line = line.split('#',1)[0] # Strip comments
            # Look for quoted key.  Key not in quotes could just be a Python var (eg c_name)
            q1 = "'%s'" % key
            q2 = '"%s"' % key
            if q1 in line or q2 in line:
                return True
    else:
        lines = subprocess.getoutput('cd %s/%s; git grep -iw %s'
                                     % (TOP, loc, key)).split('\n')
        for line in lines: # Downcase and strip comments
            line = line.lower()
            line = line.split('!',1)[0]
            line = line.split('#',1)[0]
            if key in line:
                return True
    return False

gui = 'mfixgui'
srs = 'doc/srs'
tut = 'tutorials'
leg_tut = 'legacy_tutorials'
leg_tst = 'legacy_tests'
tst = 'tests'

locs = (gui, srs, tut, tst, leg_tut, leg_tst)
cols = ('Key', 'GUI', 'SRS', 'Tut', 'Tst', 'Tut', 'Tst', 'Notes')
KEYWIDTH=35

widths = [KEYWIDTH, 7, 7, 7, 7, 7, 7, 7]

header = ' '.join(w*'=' for w in widths)

def fmt(s, l):
    l1 = min(len(s), l)
    return s[:l1] + (l-l1)*' '

# Construct table header
print(header)
print('|' + (sum(widths[:3]) + 3 - 1 )*' ' + 'Current' + (widths[3]+widths[4]+2-len('Current'))*' ' + 'Legacy')
for w in widths[:3]:
    print(w*'-', end=' ')
print((widths[3]+widths[4]+1)*'-', end=' ')
print((widths[5]+widths[6]+1)*'-', end=' ')
print(widths[7]*'-')
for c, w in zip(cols, widths):
    print(fmt(c,w), end=' ')

print()
print(header)

deprecated = {a+b for a in ('bc', 'ic', 'is', 'ps')
              for b in ('_i_e', '_i_w', '_j_n', '_j_s', '_k_b', '_k_t')}
questionable = {'c', 'frame'}

converted = {'ic_rop_s', 'bc_rop_s'}

notes = [(deprecated, 'D'),
         (questionable, '?'),
         (converted ,'C')]

for key in all_keys:
    row = [key]+[True if git_grep(loc, key) else False for loc in locs]
    if use_sqlite:
        curs.execute("INSERT INTO keywords VALUES (?,?,?,?,?,?,?)",row)
    for (x,w) in zip(row, widths):
        print(fmt('**Yes**' if x is True
                  else 'No' if x is False
                  else x, w),
              end=' ')

    for (s,n) in notes:
        if key in s:
            print(n, end='')
    print()

print(header)

print('''
Key:

- **GUI:**  Keyword is supported in MFiX-GUI
- **SRS:**  Keyword is present in SRS
- **Tut:**  Keyword is present in one or more tutorials
- **Tst:**  Keyword is present in one or more tests

Notes:

- **C**  Converted at load-time to _EP specification
- **D**  Keyword is deprecated or pending deprecation
- **?**  Table entry may be incorrect
''')

print('\nLast update:', time.ctime())

if use_sqlite:
    conn.commit()
