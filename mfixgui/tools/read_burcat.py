#!/usr/bin/env python

import os
import sys
import pickle
import time

NaN = float('NaN')

def parse_section(section):
    name = None
    comment_block = []
    data_block = []
    for (i, line) in enumerate(section):
        line = line.rstrip() # Kill trailing whitespace
        if i > 1 and 75 <= len(line) <= 80 and line[-1].isdigit() and line[-2] ==' ':
            data_block.append(line[:-2].strip())
        else:
            comment_block.append(line)

    if not data_block:
        if ' see ' in section[-1].lower():
            # it's a cross reference, we could handle these,
            # but not today
            pass
        else:
            # a few oddball entries, we're going to skip 'em
            pass
    else:
        comment = '\n'.join(comment_block)
        for i in range(int(len(data_block) / 4)):
            l1, l2, l3, l4 = data_block[i * 4:i * 4 + 4]
            name = l1[:18].strip()
            phase = l1[44]
            if phase == ' ':
                phase = l1[43]  # C2HCl5
            words = l1[45:].split()
            tmin = float(words[0])
            tmax = float(words[1])
            mol_weight = words[-1]
            # There might be some non-numeric characters abutting the last field
            while mol_weight and not mol_weight[0].isdigit():
                mol_weight = mol_weight[1:]
            mol_weight = float(mol_weight)
            coeffs = list(extract_coeffs(' '.join((l2, l3, l4))))
            if len(coeffs) == 14:
                coeffs.append(NaN)
            yield (name, phase, tmin, tmax, mol_weight, coeffs, comment)


def extract_coeffs(text):
    '''extract E15.0 formatted floats from line of text.'''
    # Note that the values may lack separating whitespace!
    text = text.upper().replace('D', 'E')  # could be lower case
    while text:
        # Look for fixed-format '0.'
        if text.startswith('0. ') or text == '0.':
            text = text[3:].strip()
            yield 0.0
        elif text.startswith('N/A'):
            text = text[3:].strip()
            yield NaN
        else:
            # This is a bit painful, since the data is not completely regular
            e = text.find('E')
            p = text.find('+')
            m = text.find('-')
            # Find the first exponent delimiter, either a + sign, -, or E
            if (0 < p) and (p < m or m <= 0) and (e == -1 or p < e):
                word, text = text[:p + 3], text[p + 3:].strip()
                word = word.replace('+', 'E+', 1)
            elif (0 < m) and (e == -1 or m < e):
                word, text = text[:m + 3], text[m + 3:].strip()
                word = word.replace('-', 'E-', 1)
            elif e > 0:
                word, text = text[:e + 4], text[e + 4:].strip()
            else:
                raise ValueError(text)
            if 'E ' in word:
                word = word.replace('E ', 'E+')  # Misformatted entry, JET-A
            if 'G' in word:  # Misformatted entry, KNO3
                word = word.replace('G', '1')

            yield float(word)


def expand_tabs(line):
    r = ''
    for c in line:
        if c == '\t':
            l = len(r)
            n = 8 * (1 + int(l / 8))
            r += ' ' * (n - l)
        else:
            r += c
    return r


def main():
    if len(sys.argv) != 3:
        print("Usage: %s infile outfile" % sys.argv[0])

    infile_name = sys.argv[1]
    outfile_name = sys.argv[2]
    if os.path.exists(outfile_name):
        print("Not clobbering %s" % outfile_name)
        sys.exit(-1)

    infile = open(infile_name, 'r', encoding='ascii')
    outfile = open(outfile_name, 'wb')

    prelude = True
    data = {}
    section = []
    for line in infile:
        line = expand_tabs(line).strip()
        if prelude:
            if line.startswith('THERMO DATA'):
                prelude = False
            continue

        if "REST IN PEACE" in line:
            time.sleep(1)

        if line.startswith("Troughout"):  # sic
            # Done, we reached the comments at end of file
            break

        if line:
            section.append(line)
        else:  # sections separated by blanks
            if not section:  # extra blank lines
                continue
            for (name, phase, tmin, tmax, mol_weight, coeffs, comment) in parse_section(section):
                key = (name, phase, tmin, tmax)
                if key in data:
                    # uniquify names? not now, just skip
                    print("duplicate key %s, skipping" % str(key))
                else:
                    data[key] = (coeffs, mol_weight, comment)
            section = []

    pickle.dump(data, outfile)


if __name__ == '__main__':
    main()
