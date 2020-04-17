#!/usr/bin/env python

"""
Filter to collect output from 'conda build' and append a summary
of error/warning messages to the output.  Make sure the shell
'pipefail' option is set when using this, so that the exit status
from 'conda build' will be lost.
"""

import sys
import re

def plural(n, word):
    fmt = "%d %s" if n == 1 else "%d %ss"
    return fmt % (n, word)

# Require space or colon after 'error', so we don't match things
# like 'error_manager.f'
filters = (('warning',  re.compile(r'\bwarning\b', re.IGNORECASE), []),
           ('error', re.compile(r'\berror\b', re.IGNORECASE), []))

for line in sys.stdin:
    for (name, pat, lines) in filters:
        if pat.search(line):
            lines.append(line)

print("************ERROR/WARNING SUMMARY************")

LIMIT=10
for (name, pat, lines) in filters:
    print(plural(len(lines), name))
    for line in lines[:LIMIT]:
        sys.stdout.write(line)
        if len(lines) > LIMIT:
            print(plural(LIMIT-len(lines), name), "skipped")
