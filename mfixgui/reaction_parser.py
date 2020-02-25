#!/usr/bin/env python

from collections import OrderedDict

# key := string | string (  index )
# val := string | string & val
# expr :=  key = val
# reaction := reaction ID  { expr [expr]+ }

class ParseError(Exception):
    pass


class ReactionParser(object):
    def __init__(self):
        self.state = 0
        self.reactions = OrderedDict()

    def emit(self):
        # Turn values into numbers, where possible
        def to_float(val):
            vtmp = val.lower().replace('d', 'e')
            try:
                return float(vtmp)
            except ValueError:
                return val

        for (k, v) in self.d.items():
            if isinstance(v, dict):
                for (k2, v2) in v.items():
                    v[k2] = to_float(v2)
            else:
                self.d[k] = to_float(v)

        self.reactions[self.reaction_id] = self.d
        chem_eq = self.d.get('chem_eq','NONE')
        self.d['reactants'], self.d['products'] = self.parse_chem_eq(chem_eq)
        if chem_eq.upper() != "NONE":
            # Reformat chemical equation to canonicalize format
            fmt = {}
            for side in 'reactants', 'products':
                fmt[side] = ' + '.join(species if coeff==1.0 else '%g*%s' % (coeff, species)
                                       for (species, coeff) in self.d[side] if coeff)
            chem_eq = "%s --> %s" % (fmt['reactants'], fmt['products'])
            self.d['chem_eq'] =chem_eq

    def parse(self, data):
        for t in self.tokenize(data):
            self.push(t)

    def tokenize(self, data):
        for line in data.split('\n'):
            line = line.strip()
            tok = ''
            quote = None
            for c in line:
                if c == quote: # matched quotes
                    quote = None
                    if tok:
                        yield tok
                        tok = ''
                elif c in ('"', "'"):
                    quote = c
                elif c in '{}()=':
                    if tok:
                        yield tok
                        tok = ''
                    yield c
                elif c in ('!', '#') and not quote:
                    if tok:
                        yield tok
                        tok = ''
                    break
                elif quote:
                    tok += c
                elif c.isspace():
                    if tok:
                        yield tok
                        tok = ''
                else:
                    tok += c
            if tok:
                yield tok
                tok = ''


    def push(self, t):
        if self.state == 0: # start of reaction
            self.d = {}
            self.key = ''
            self.index = None
            if not self.valid_reaction_id(t):
                self.err('reaction id', t)
            self.reaction_id = t
            self.state = 1
        elif self.state == 1: # start of { block
            if t != '{':
                self.err('{', t)
            self.state = 2
        elif self.state == 2: # start of key
            if not self.is_alnum(t):
                self.err('key', t)
            self.key = t.lower()
            self.index = None
            if self.key not in self.d:
                self.d[self.key] = ''
            self.state = 3
        elif self.state == 3: # key may have args
            if t == '(':
                self.state = 4
            elif t == '=':
                self.state = 7
            else:
                self.err('( or =', t)
        elif self.state == 4: # start of arg
            if not t.isdigit():
                self.err('integer', t)
            self.index = int(t)
            self.state = 5
        elif self.state == 5: # expect closing paren
            if t != ')':
                self.err(')', t)
            self.state = 6
        elif self.state == 6: # expect =
            if t != '=':
                self.err('=', t)
            self.state = 7
        elif self.state == 7: # start of value or contination of value
            if t in '{}()=':
                self.err('string value', t)
            if self.key not in self.d:
                self.err('key', t)
            if self.index is not None:
                if self.d[self.key] == '':
                    self.d[self.key] = {}
                if self.index not in self.d[self.key]:
                    self.d[self.key][self.index] = ''
                self.d[self.key][self.index] += t
            else:
                self.d[self.key] += t
            self.state = 8
        elif self.state == 8: # end of reaction block or continuation line
            if t == '&':
                self.state = 7
            else:
                if t == '}':
                    self.emit()
                    self.state = 0
                elif not self.is_alnum(t):
                    self.err('key', t)
                else:
                    self.key = t.lower()
                    if self.key not in self.d:
                        self.d[self.key] = ''
                    self.state = 3


    def is_alnum(self, s):
        return all((c.isalnum() or c=='_') for c in s)


    def valid_reaction_id(self, s):
        if s in self.reactions:
            print("reaction_id %s already defined" % s)
        return len(s) <= 32 and self.is_alnum(s) and s not in self.reactions


    def err(self, expected, got):
        raise ParseError('expected %s, got %s state=%s' % (expected, got, self.state))


    def parse_chem_eq(self, chem_eq):
        """parse chemical equation, return two lists (Reactants, Products)
        each list is a list of tuples (species, coefficient)"""
        if chem_eq is None or chem_eq.upper()=='NONE':
            return [], []
        if '-->' in chem_eq:
            lhs, rhs = chem_eq.split('-->', 1)
        elif '==' in chem_eq:
            lhs, rhs = chem_eq.split('==', 1)
        else:
            raise ParseError('expected --> or == in chem_eq, got %s' % chem_eq)

        def parse_side(side):
            ret = []
            for field in side.split('+'):
                field = field.strip()
                if not field:
                    continue # ?
                if '*' in field:
                    coeff, species = field.split('*', 1)
                    ret.append([species.strip(), float(coeff)])
                elif ' ' in field:
                    coeff, species = field.split(' ', 1)
                    ret.append([species.strip(), float(coeff)])
                else:
                    coeff = ''
                    while field and field[0].isdigit() or field[0]=='.':
                        coeff += field[0]
                        field = field[1:]
                    if coeff:
                        coeff = float(coeff)
                    else:
                        coeff = 1.0
                    ret.append([field.strip(), coeff])
            return ret

        return parse_side(lhs), parse_side(rhs)


def main():
    data = """
Phase_Change { chem_eq = "Solid --> Gas" }
Drying { chem_eq = "Liquid --> Vapor" }
Char_Combustion1 { chem_eq = "2FC1 + O2 --> 2CO"}
Char_Combustion2 { chem_eq = "2FC2 + O2 --> 2CO"}
Char_Combustion3 { chem_eq = '2.2FC1 + O2 --> 2CO + 0.2sSoot'}
CH4_Combustion { chem_eq = "CH4 + 2O2 --> CO2 + 2H2O" }
CH4_Catalytic1 { chem_eq = "CH4 + 2O2 + 0.FC1 --> CO2 + 2H2O" }
CH4_Catalytic2 { chem_eq = "CH4 + 2O2 --> CO2 + 2H2O + 0.FC1" }
CH4_Catalytic3 { chem_eq = "CH4 + 2O2 + 0.FC1 --> CO2 + 2H2O + 0.FC1" }
Char_to_Char { chem_eq = 'FC1 --> FC2' }  ! Hot --> Cold
Ash_to_Ash { chem_eq = 'Ash2 --> FlyAsh' } ! Cold --> Hot
Skippy_RxN { chem_eq = 'None' }
Ozone_Decomp { chem_eq = "O3 --> 1.5O2" }
RX1F { chem_eq = "SiH4 --> SiH2 + H2"}
RX1R { chem_eq = "SiH2 + H2 --> SiH4"}
RX2F { chem_eq = "Si2H6 --> SiH4 + SiH2"}
RX2R { chem_eq = "SiH4 + SiH2 --> Si2H6"}
RX3  { chem_eq = "SiH4 --> Si + 2H2"}
RX4  { chem_eq = "SiH2 --> Si + H2"}
CHAR_COMBUSTION { chem_eq = "2FC1 + O2 --> 2CO"}
CHAR_CO2  { chem_eq = "FC1 + CO2 --> 2CO"}           ! Forward
CHAR_CO2r { chem_eq = "2CO + 0.FC1 --> Soot + CO2"}  ! Reverse
CO_Combustion { chem_eq = "CO + 0.5O2 --> CO2" }
Combustion_s1 { chem_eq = "2FC1 + O2 --> 2CO"}
Combustion_s2 { chem_eq = "2FC2 + O2 --> 2CO"}
Char_CO2_s1 { chem_eq = "FC1 + CO2 --> 2CO"}            ! Forward
Char_CO2_s1r { chem_eq = "2CO + 0.FC1 --> Soot + CO2"}  ! Reverse
Char_CO2_s2 { chem_eq = "FC2 + CO2 --> 2CO"}            ! Forward
Char_CO2_s2r { chem_eq = "2CO + 0.FC2 --> Soot + CO2"}  ! Reverse
CO_Combustion1 { chem_eq = "CO + 0.5O2 --> CO2" }
Char_to_Char1 { chem_eq = 'FC2 --> FC1' }
Ash_to_Ash1{ chem_eq = 'Ash2 --> Ash1' }
Drying1 { chem_eq = 'Moisture --> H2O' }
Pyrolysis {
chem_eq = 'Biomass --> ' &
'0.9639 * CO + 0.8771 * CO2 + 0.3491 * CH4 + ' &
'1.6276 * H2 + 1.4210 * H2O'
DH = 3.585d3     ! (cal/mol-biomass)  150.0 J/g-biomass
fracDH(1) = 1.0  ! assign to the coal phase
}
Charring {
chem_eq = 'Biomass --> 8.3334 * Char'
DH = 3.585d3     ! (cal/mol-biomass)  150.0 J/g-biomass
fracDH(1) = 1.0  ! assign to the coal phase
}
Tarring {
chem_eq = 'Biomass --> Tar'
DH = 3.585d3     ! (cal/mol-biomass)  150.0 J/g-biomass
fracDH(1) = 1.0  ! assign to the coal phase
}
AtoR { chem_eq = "A --> 3R" }
"""
    rp = ReactionParser()
    for t in rp.tokenize(data):
        rp.push(t)


if __name__ == '__main__':
    main()
