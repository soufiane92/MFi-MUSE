"""
project.py
==========

This module defines Project and Keyword classes.

A Project holds Keyword settings representing an mfix project.
It can read/write files in 'mfix.dat' format

Keyword values are coerced to lower-case.

Keywords can take arguments, which are typically indices representing
phase number, eg   momentum_x_eq(0)  for phase 0.   Some keywords take
multiple arguments.

Keyword objects have a 'key' (the string) and a 'value'.

There are three paths to the Keyword objects in the Project:
 dict, list, and Collections

 list for preserving the order for writing
 dict for programmatically changing keywords
 collections for user interaction with the keywords
"""

import sys
import math
import re
import warnings
import traceback
from collections import OrderedDict
from json import JSONDecoder, JSONEncoder
import shlex

from io import StringIO

from mfixgui.tools.simpleeval import simple_eval
from mfixgui.tools.comparable import Comparable
from mfixgui.tools import format_key_with_args

from mfixgui.reaction_parser import ReactionParser

from mfixgui.regexes import *
from mfixgui.constants import *

NaN = float('NaN')

PYTHON_TYPE_DICT = {'float':float, 'int':int, 'bool':bool}
PYTHON_TYPE_DICT_REVERSE = dict([(val, key) for key,val in PYTHON_TYPE_DICT.items()])

# Canonicalize long BC_TYPE to short form, don't worry about about long/short versions later
BC_TYPE_DICT = {
    #'DUMMY': None,
    'MASS_INFLOW': 'MI',
    'MASS_OUTFLOW': 'MO',
    'P_INFLOW': 'PI',
    'P_OUTFLOW': 'PO',
    'FREE_SLIP_WALL': 'FSW',
    'NO_SLIP_WALL': 'NSW',
    'PAR_SLIP_WALL': 'PSW',
}

# For IS_TYPE, prefer the long form (?)
IS_TYPE_DICT = {
    'IP': 'IMPERMEABLE',
    'SP': 'SEMIPERMEABLE',
}

class FloatExp(float):
    fmt = '4'
    def __repr__(self):
        return '{:.{}e}'.format(self, self.fmt)

    __str__ = __repr__

def make_FloatExp(val):
    """ Handle Fortran formats """
    # (we can't do this in FloatExp.__init__)
    try:
        return FloatExp(val)
    except ValueError:
        val = val.lower().replace('d', 'e')
        if val.endswith('e'):
            val += "+0"
        return FloatExp(val)


def clean_string(string):
    """Attempt to clean strings of '," and convert .t., .f., .true., .false.
    to booleans, and catch math expressions i.e. @(3*3) and remove the @()"""

    if not string:
        return ''

    quoted = string[0] in ('"', "'")
    # remove all quotes
    string = string.replace("'", '').replace('"', '')
    # remove any leading or trailing space, after removing quotes
    string = string.strip()
    # Remove comma separators if present
    if string.endswith(','):
        string = string[:-1]

    # lower-case version of string
    s_lower = string.lower()

    if not quoted:
        if s_lower in ('.t.', '.true.'):
            return True
        elif s_lower in ('.f.', '.false.'):
            return False

        # Look for @() expression
        if string.startswith('@(') and string.endswith(')'):
            return Equation(string)

        # Look for exponential-notation
        match = re_float_exp.match(string)
        if match:
            return make_FloatExp(string)

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

def expand_shorthand(string):
    """Expand mfix.dat shorthand:
    expand_shorthand("bill 5*'IKE' fred") = "bill 'IKE' 'IKE' 'IKE' 'IKE' 'IKE' fred"
    But don't do it inside parenthesized expressions.
    """
    # TODO:  we could do better about handling quoted strings
    # and escapes
    def _expand(s): # Inner function to do expansion, once we've handled quoting and parens
        for shorthand in re_shorthand.findall(s):
            count, word = shorthand.split('*', 1)
            count = int(count)
            expansion = ' '.join(count * [word])
            s = s.replace(shorthand, expansion)
        return s
    paren_level = 0
    ret = []
    part = ''
    for c in string:
        if c == '(':
            paren_level += 1
            if paren_level == 1: # Just entered paren. context
                ret.append(_expand(part))
                part = ''
        if c == ')':
            paren_level -= 1
            if paren_level == 0:
                ret.append(part+c)
                part = ''
        else:
            part += c
    if part: # leftover at end
        ret.append(_expand(part))
    ret = ''.join(part for part in ret if part)
    return ret


def remove_spaces_from_equations(string):
    # Can't do this with regex, so here's a simple lexical scanner.
    quote = None
    escape = False
    paren_level = 0
    in_eq = False
    ret = ''
    for c in string:
        if c == '\\':
            escape = not escape
        else:
            if quote and c == quote and not escape:
                quote = False
            elif c == '"' or c == "'": # Start of quoted text
                                # (We're not doing triple-quotes!)
                quote = c
            elif not (quote or escape):
                if c == '@':
                    in_eq = True
                if c == '(':
                    paren_level += 1
                if c == ')':
                    paren_level -= 1
                    if paren_level == 0:
                        in_eq = False

        if not(in_eq and c.isspace()):
            ret += c

    return ret


# --- json extension ---
class ExtendedJSON(object):
    @staticmethod
    def dumps(obj):
        return EquationAwareJSONEncoder().encode(obj)

    @staticmethod
    def loads(string):
        if string in ("None", None):
            return {}
        else:
            return EquationAwareJSONDecoder().decode(string)


class EquationAwareJSONEncoder(JSONEncoder):
    """
    JSONEncoder that handles Equation objects
    """
    def default(self, obj): # pylint: disable=method-hidden
        if isinstance(obj, Equation):
            return {
                '__type__' : 'equation',
                'eq' : obj.eq,
                'type' : PYTHON_TYPE_DICT_REVERSE[obj.dtype],
                }
        else:
            return JSONEncoder.default(self, obj)


class EquationAwareJSONDecoder(JSONDecoder):
    """
    JSONDecoder that handles Equation onjects.
    """

    def __init__(self):
        JSONDecoder.__init__(self, object_hook=self.dict_to_object)

    def dict_to_object(self, d):
        if '__type__' not in d:
            return d

        type = d.pop('__type__')
        if type == 'equation':
            return Equation(d['eq'], PYTHON_TYPE_DICT[d['type']])
        else:
            # Oops... better put this back together.
            d['__type__'] = type
            return d


# --- project base ---
class Equation(object):
    """represents a simple arithmetic expression, which can be evaluated
    by simple_eval, in a namespace which includes the math constants 'e'
    and 'pi'.  Calling "float" causes evaluation, which will result in
    ValueError on any failures.  Evaluating an empty string returns 0.0,
    while evaluating None returns NaN"""

    def __init__(self, eq, dtype=float):
        eq = str(eq)
        while eq.startswith("@(") and eq.endswith(")"):
            eq = eq[2:-1]
        # TODO: don't use this hack to store dtype in equation string
        if ',' in eq:
            eq, dtype = eq.split(',')
            dtype = PYTHON_TYPE_DICT[dtype]
        self.eq = eq
        self.dtype = dtype

    def get_used_parameters(self):
        av_params = PARAMETER_DICT.keys()
        eq = re.split('[\*\/\-\+ \(\)]', self.eq)
        return [p for p in av_params if p in eq]

    def check_parameters(self):
        return list(set(self.get_used_parameters())-set(PARAMETER_DICT.keys()))

    def _eval(self):
        if len(self.eq) == 0:
            return 0
        elif self.eq is None or self.eq == 'None':
            return NaN
        elif len(self.check_parameters()) > 0:
            raise ValueError('invalid parameter(s): {}'.format(','.join(self.check_parameters())))
        else:
            name_dict = {}
            # only collect used parameters
            for key in self.get_used_parameters():
                value = PARAMETER_DICT.get(key, None)
                if value is self:
                    raise ValueError('Circular reference.')
                try:
                    name_dict[key] = float(value)
                except ValueError:
                    # could have been a string parameter
                    pass
            try:
                return float(simple_eval(self.eq,
                                         names=name_dict))

            except:
                raise ValueError(self.eq)

    def __nonzero__(self):  # Python 2
        return not math.isnan(self._eval())

    __bool__ = __nonzero__  # Python 3

    def __float__(self):
        return float(self._eval())

    def __int__(self):
        # Will raise ValueError if equation evaluates to NaN
        return int(self._eval())

    def __repr__(self):
        return ''.join(['@(', str(self.eq), ')'])

    def __round__(self, n):
        return self

    def dumps(self):
        return '%s #!MFIX-GUI eq{%s}' % (
            self.dtype(self._eval()), ','.join([self.eq, PYTHON_TYPE_DICT_REVERSE[self.dtype]]))

    def __cmp__(self, other): # Python2
        if other is None:
            return 1
        if isinstance(other, (float, int, Equation)): # Value comparison
            a = float(self)
            b = float(other)
        else: # String comparison
            a = self.eq
            b = other
        return -1 if a<b else 1 if a>b else 0

    def __lt__(self, other):
        return self.__cmp__(other) < 0

    def __gt__(self, other):
        return self.__cmp__(other) > 0

    def __eq__(self, other):
        return self.__cmp__(other) == 0

    # Symbolic math on equations
    def binop(self, x, c):
        if isinstance(x, Equation):
            return Equation('(%s)%s(%s)' % (self.eq, c, x.eq))
        else:
            return Equation('(%s)%s%s' % (self.eq, c, x))

    def r_binop(self, x, c=''):
        if isinstance(x, Equation):
            return Equation('(%s)%s(%s)' % (x.eq, c, self.eq))
        else:
            return Equation('%s%s(%s)' % (x, c, self.eq))

    def __add__(self, x):
        return self.binop(x, '+')

    def __radd__(self,x):
        return self.r_binop(x, '+')

    def __sub__(self, x):
        return self.binop(x, '-')

    def __rsub__(self,x):
        return self.r_binop(x, '-')

    def __mul__(self, x):
        return self.binop(x, '*')

    def __rmul__(self,x):
        return self.r_binop(x, '*')

    def __div__(self, x):
        return self.binop(x, '/')
    __truediv__ = __floordiv__ = __div__

    def __rdiv__(self, x):
        return self.r_binop(x, '/')
    __rtruediv__ = __rfloordiv__ = __rdiv__

    def __pow__(self, x):
        return self.binop(x, '**')

    # unary operations
    def unaryop(self, c):
        return Equation('%s(%s)' % (c, self.eq))

    def __neg__(self):
        return self.unaryop('-')

    def __pos__(self):
        return self.unaryop('+')

    def __abs__(self):
        return Equation('abs(%s)' % (self.eq))


class Keyword(Comparable):
    def __init__(self, key, val, comment='', dtype=None, args=None):

        self.key = key
        self.value = val
        self.dtype = dtype
        self.min = None
        self.max = None
        self.valids = None
        self.fmt = None
        self.comment = comment
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []
        self.args = args
        assert isinstance(args, list)

        if dtype is None:
            self.update_dtype()

    def __float__(self):
        try:
            return float(self.value)
        except (ValueError, TypeError, ZeroDivisionError):
            return NaN # !?

    def __int__(self):
        return int(self.value)

    def _cmpkey(self):
        return self.value

    def __nonzero__(self): # python 2
        if self.dtype == bool:
            return bool(self.value)
        elif self.value is None:
            return False
        elif self.value == '':
            return False
        elif self.dtype == str:
            return True
        elif math.isnan(float(self)): # Failed expression evaluation.
            return False
        else:
            return True

    __bool__ = __nonzero__ # python 3

    def __str__(self):
        sval = str(self.value)
        if self.dtype == FloatExp:
            return sval
        elif self.dtype == Equation:
            return sval
        elif self.dtype == float:
            return sval
        elif self.dtype == int:
            return sval
        elif self.dtype == bool:
            return '.%s.' % sval
        elif self.dtype == str and self.value is not None:
            sval = sval.replace('"', '').replace("'", '')
            return "'%s'" % sval
        else:
            return sval

    def update_dtype(self):
        # Check data type & modify if needed
        for dtype in (bool, float, int, FloatExp, Equation):
            if isinstance(self.value, dtype):
                self.dtype = dtype
                break

        # If still None, assume string
        if self.dtype is None:
            self.dtype = str

    def line(self):
        if self.dtype == Equation:
            val = self.value.dumps()
        else:
            val = str(self)

        if len(self.args) == 0:
            line = '  {} = {}'.format(self.key, val)
        else:
            line = '  {}({}) = {}'.format(self.key,
                                          ','.join(str(x) for x in self.args),
                                          val)

        if self.comment:
            line = '    !'.join([line, self.comment])

        return line

    def updateValue(self, value):
        sval = str(value)
        if value is None or sval == '': # is this the right place to check this?
            self.value = None
        elif self.dtype == Equation and isinstance(value, str):
            self.value.eq = value
        elif self.dtype == float and not re_float.match(sval) and not re_int.match(sval):
            if re_float_exp.match(sval):
                self.value = make_FloatExp(value)
            else:
                eq = Equation(value)
                try:
                    float(eq)
                    self.value = eq
                except ValueError:
                    pass # Don't update invalid equation

        elif self.dtype == FloatExp and (isinstance(value, (int, float)
                                                    and not isinstance(value, bool))):
            self.value = make_FloatExp(value)
        else:
            self.value = value
        # TODO> make sure we don't change to invalid type
        self.update_dtype()

    def lower(self):
        return self.value.lower()


class Base(object):
    def __init__(self, ind):
        self.ind = ind # index
        self.keyword_dict = {}

    def __getitem__(self, name):
        return self.keyword_dict[name]

    def __setitem__(self, name, value):
        if name in self.keyword_dict and isinstance(self.keyword_dict[name],
                                                    Keyword):
            if isinstance(value, Keyword):
                self.keyword_dict[name].updateValue(value.value)
            else:
                self.keyword_dict[name].updateValue(value)
        else:
            self.keyword_dict[name] = value

    def __contains__(self, item):
        return item in self.keyword_dict

    def __len__(self):
        return len(self.keyword_dict)

    def get(self, key, default=None):
        # Note, this only works with dynamic attributes, not static ones defined
        # in subclasses of Base (eg Solid.name)
        d = self.keyword_dict.get(key)
        return default if d is None else d.value


class CondBase(Base):
    def __init__(self, ind):
        Base.__init__(self, ind)

        self.gasSpecies = SpeciesCollection()
        self.solids = SolidsCollection()


class BC(CondBase):
    def __init__(self, ind):
        CondBase.__init__(self, ind)
        self.name = 'BC'

    def __str__(self):
        if self.gasSpecies:
            gasSpec = ['  Gas Species:'] + [''.join(['    ', val]) for val in
                                            self.gasSpecies.prettyPrintList()]
        else:
            gasSpec = []

        if self.solids:
            solids = ['  Solids:'] + self.solids.prettyPrintList()
        else:
            solids = []

        return '\n'.join(
            ["BC %s:" % self.ind]
            + [''.join('  %s: %s' % (k, v)
                       for (k,v) in self.keyword_dict.items())]
            + gasSpec
            + solids)


class IC(CondBase):
    def __init__(self, ind):
        CondBase.__init__(self, ind)
        self.name = 'IC'

    def __str__(self):
        return "IC %s" % self.ind


class PS(CondBase):
    def __init__(self, ind):
        CondBase.__init__(self, ind)
        self.name = 'PS'

    def __str__(self):
        return "PS %s" % self.ind


class IS(CondBase):
    def __init__(self, ind):
        CondBase.__init__(self, ind)
        self.name = 'IS'


class VTK(CondBase):
    def __init__(self, ind):
        CondBase.__init__(self, ind)
        self.name = 'VTK'

    def __str__(self):
        return "VTK %s" % self.ind


class ConditionCollection(list):
    def __init__(self, condtype=None):
        list.__init__(self)
        self.condtype = condtype

    def __contains__(self, item):
        if item in [itm.ind for itm in self]:
            return True
        else:
            return False

    def __getitem__(self, item):
        for itm in self:
            if itm.ind == item:
                return itm

        if self.condtype == 'ic':
            self.append(IC(item))
        elif self.condtype == 'bc':
            self.append(BC(item))
        elif self.condtype == 'ps':
            self.append(PS(item))
        elif self.condtype == 'is':
            self.append(IS(item))
        elif self.condtype == 'vtk':
            self.append(VTK(item))

        else:
            raise Exception("Collection Type Not Set")

        return self[item]

    def delete(self, itm):
        self.pop(self.index(itm))


class Species(Base):
    """
    Class to hold properties and functions for gas and solid species.

    Usage: Species(index, phase)
        index = int
        phase = 'g' for gas, or 's' for solid, or l for liquid.
    """
    def __init__(self, ind, phase='g'):
        Base.__init__(self, ind)
        self.phase = phase

    def __str__(self):
        if self.phase == 'g':
            p = 'Gas'
        else:
            p = "Solid"
        return '\n'.join(["{} Species: {}".format(p, self.ind), ] +
                         [''.join(['  ', str(key), ': ', str(value)])
                          for key, value in self.keyword_dict.items()])


class Solid(Base):
    def __init__(self, ind):
        Base.__init__(self, ind)

        self.species = SpeciesCollection()
        self.name = 'Solid {}'.format(self.ind)

    def get(self, key, default=None):
        return self.keyword_dict.get(key, default)

    def addSpecies(self, ind):
        return self.species.new(ind, phase='s')

    def prettyPrintList(self):
        solidAttr = [''.join(['  ', str(key), ': ', str(value)])
                     for key, value in self.keyword_dict.items()]

        lst = [self.name] + solidAttr +\
              ['  Species:'] + [''.join(['    ', val]) for val in
                                self.species.prettyPrintList()]
        return lst

    def __str__(self):
        return '\n'.join(self.prettyPrintList())


class Collection(list):
    def __init__(self):
        list.__init__(self)
        self.indStart = 1

    def __getattr__(self, name):
        if name in self:
            for item in [itm.name for itm in self]:
                return item
        else:
            # Default behaviour
            raise AttributeError(name)

    def __contains__(self, item):
        if item in [itm.ind for itm in self]:
            return True
        else:
            return False

    def __getitem__(self, item):
        for itm in self:
            # Slow - O(n^2).  But we're not really using this.
            if itm.ind == item:
                return itm

    def check_ind(self, ind):
        # Check index for valid range
        currentSet = [itm.ind for itm in self]
        if ind in currentSet:
            raise Exception("An index of {} already exists".format(ind))

        if ind < self.indStart:
            raise Exception("An index of {} not allowed. "
                            "Range starts at {}".format(ind, self.indStart))

        if ind is None:
            if len(currentSet) < 1:
                ind = 1
            else:
                full_set = set(range(1, max(currentSet) + 1))
                ind = sorted(full_set - currentSet)[0]

        return ind

    def clean(self):
        for itm in self:
            if hasattr(itm, 'keyword_dict') and not itm.keyword_dict:
                self.pop(self.index(itm))

    def delete(self, itm):
        self.pop(self.index(itm))


class SpeciesCollection(Collection):
    def __init__(self):
        Collection.__init__(self)

    def new(self, ind=None, phase='g'):
        ind = self.check_ind(ind)
        self.append(Species(ind, phase))
        return self[ind]

    def prettyPrintList(self):
        printDict = {}
        for spec in self:
            if hasattr(spec, 'species_alias_s'):
                printDict[spec.ind] = spec.species_alias_s
            elif hasattr(spec, 'species_alias_g'):
                printDict[spec.ind] = spec.species_alias_g
            else:
                printDict[spec.ind] = None

        return [' '.join([str(key), str(val)])
                for key, val in printDict.items()]

    def __str__(self):
        return '\n'.join(self._prettyPrintList())


class SpeciesEq(Base):
    def __init__(self, ind):
        Base.__init__(self, ind)


class SpeciesEqCollection(Collection):
    def __init__(self):
        Collection.__init__(self)
        self.indStart = 0

    def new(self, ind=None):
        ind = self.check_ind(ind)
        self.append(SpeciesEq(ind))
        return self[ind]


class SolidsCollection(Collection):
    def __init__(self):
        Collection.__init__(self)

    def new(self, ind=None):
        ind = self.check_ind(ind)
        self.append(Solid(ind))
        return self[ind]

    def _prettyPrintList(self):
        print_list = []
        for solid in self:
            print_list += solid._prettyPrintList()

        return print_list

    def __str__(self):
        return '\n'.join(self._prettyPrintList())


class VariableGridVar(Base):
    def __init__(self, ind):
        Base.__init__(self, ind)


class VariableGridCollection(Collection):
    def __init__(self):
        Collection.__init__(self)
        self.indStart = 0

    def new(self, ind=None):
        ind = self.check_ind(ind)
        self.append(VariableGridVar(ind))
        return self[ind]


class Project(object):
    """holds keywords and thermodynamic data for an MFiX project,
    reads and writes project file"""

    def __init__(self, dat_file=None, keyword_doc=None):

        self.dat_file = dat_file
        self.keyword_doc = keyword_doc
        self.comments = {}
        self.keyword_dict = {}
        self.dat_file_list = [] # contains the project file, lines are replaced with
                            # keywords as parsed
        self.thermo_data = {} # key=species value=list of lines
        self.mfix_gui_comments = OrderedDict() # lines starting with #!MFIX-GUI
        self.parameter_key_map = {}  # key=parameter, value=set of keywords
        self.reactions = OrderedDict()
        # See also 'reset'

        self.init_data()

        if self.dat_file is not None:
            self.parsemfixdat()

    def init_data(self):
        # Conditions
        self.ics = ConditionCollection('ic')
        self.bcs = ConditionCollection('bc')
        self.pss = ConditionCollection('ps')
        self.iss = ConditionCollection('is')

        # VTK output regions
        self.vtks = ConditionCollection('vtk')

        # Species/solids
        self.gasSpecies = SpeciesCollection()
        self.solids = SolidsCollection()
        self.speciesEq = SpeciesEqCollection()

        # variablegrid
        self.variablegrid = VariableGridCollection()

#    def __getattr__(self, name):
#        raise DeprecationWarning('__getattr__(%s)' % name)
#
#    def __getitem__(self, key):
#        raise DeprecationWarning('__getitem__(%s)' % key)

    def get(self, key, args=None):
        """get(key, args=None): returns a keyword object, if defined, else None"""
        key = key.lower()
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []
        r = self.keyword_dict.get(key)
        if r is None:
            return None
        r = r.get(tuple(args))
        return r

    def put(self, key, keyword, args=None):
        """add a keyword to project."""
        key = key.lower()
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []
        if not isinstance(keyword, Keyword):
            raise TypeError(keyword)
        if key not in self.keyword_dict:
            self.keyword_dict[key] = {}
        self.keyword_dict[key][tuple(args)] = keyword
        self.dat_file_list.append(keyword) # placement?

    def get_value(self, key, default=None, args=None):
        """get_value(key, default=None, args=None): return value of keyword
        object, else default.  'default' and 'args' should be keyword args"""
        r = self.get(key, args)
        return default if r is None else r.value


    def get_key_indices(self, key):
        """return a list of indices (args) for which the key is defined"""
        # Returning as a list makes this safe to iterate over, even if keys are
        # added/deleted
        # TODO: Should we make each argument tuple a list?  Probably
        return sorted(list(self.keyword_dict.get(key, {}).keys()))


    def removeKeyword(self, key, args=None, warn=True):
        """Remove a keyword from the project. (keyword_dict and dat_file_list)
        return True if item deleted.
        if warn=True, raise KeyError if item not present, else return False."""
        key = key.lower()
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []
        if isinstance(args, tuple):
            args = list(args) # implementation detail, if tuple was
            # passed we need it to match a list
        try:
            r = self.keyword_dict[key]
        except KeyError:
            if warn:
                raise
            else:
                return False
        try:
            del r[tuple(args)]
        except KeyError:
            if warn:
                raise
            else:
                return False
        if not self.keyword_dict[key]: # empty sub-dict
            del self.keyword_dict[key]

        # remove from dat_file_list
        # Note. list.remove does not work because of the way keyword comparison is implemented,
        #  since remove will remove the first key with matching value
        self.dat_file_list = [k for k in self.dat_file_list
                              if not (isinstance(k, Keyword)
                                      and k.key==key and k.args==args)]

        return True


    def parsemfixdat(self, fname=None):
        """Parse the mfix.dat file with regular expressions.
        fname can be a StringIO instance, path, or a plain string.
        """
        # TODO. move this to another module
        #  maybe write a real tokenizer and grammar.
        if fname:
            self.dat_file = fname
        assert self.dat_file is not None

        # check to see if the file is a StringIO object
        if isinstance(self.dat_file, StringIO):
            self.parsemfixdat_inner(self.dat_file)
            return

        # Try to open the file
        try:
            with open(self.dat_file, 'r', encoding='utf-8', errors='ignore') as dat_file:
                self.parsemfixdat_inner(dat_file)
            return
        except (IOError, OSError):
            pass
        # maybe it is just a string?
        if isinstance(self.dat_file, str):
            self.parsemfixdat_inner(StringIO(self.dat_file))
            return

    def parseKeywordLine(self, line, equation_str=None):
        if not line.strip():
            yield (None, None, None)
            return
        matches = re_keyvalue.findall(line)
        single_key = False
        if matches:
            for match in matches:
                # match could be: [keyword, args, value,
                #                   nextKeywordInLine, something]

                # convert to list so we can reassign
                match = list(match)

                # keyword
                key = match[0].lower().strip()

                # values
                val_string = match[2].strip()

                # remove spaces from equations: @( 2*pi)
                # (doing this with regex won't work because it can't detect
                # balanced parens)
                val_string = remove_spaces_from_equations(val_string)

                # look for shorthand [count]*[value] and expand.
                val_string = expand_shorthand(val_string)

                # split values using shlex, it will keep quoted strings together.
                if 'description' in key: # Don't do anything with description line, it may contain *, etc.
                    vals = [shlex.split(line.strip(), posix=False)[-1]] # Line cannot be empty string here
                    single_key = True
                else:
                    try:
                        if val_string.strip(): # shlex.split(posix=False) cannot handle empty string)
                            vals = shlex.split(val_string.strip(), posix=False) # posix=False preserves quotes
                        else:
                            vals = []

                    except ValueError:
                        vals = []

                # shlex.split("'P0', 'P1'") results in ['P0', ',' 'P1']
                vals = [v for v in vals if v != ',']

                # if equation in comment, replace last value
                if equation_str is not None and vals:
                    vals[-1] = '@(' + equation_str +')'

                # clean the values converting to python types
                cleanVals = []
                for val in vals:
                    val = clean_string(val)
                    if val is not None:
                        cleanVals.append(val)

                # check keyword_doc to see if the keyword is an array
                if (self.keyword_doc is not None and key in self.keyword_doc
                        and not match[1] and self.keyword_doc[key]['args']):
                    match[1] = ','.join(str(arg['min']) for arg in
                                        self.keyword_doc[key]['args'].values())

                # clean up arguments
                args = []
                colon_arg = None # if one of the args is of the form lo:hi
                colon_lo = None
                colon_hi = None
                if match[1]:
                    for (i, arg) in enumerate(match[1].split(',')):
                        if ':' in arg:
                            if colon_arg is not None: # Only one index can have :
                                raise ValueError(match[1])
                            colon_arg = i
                            colon_lo, colon_hi = map(int,
                                                     arg.split(':'))
                            args.append(colon_lo)
                        else:
                            args.append(int(arg))

                numVals = len(cleanVals)

                if colon_arg is not None:
                    expect = 1 + colon_hi - colon_lo
                    if numVals != expect:
                        raise ValueError('Expected %s values, got %s' % (expect, numVals))

                # If multiple values, split apart into multiple key, args, values

                if numVals > 1:
                    keywordArgList = []

                    if numVals > 1:
                        if colon_arg is not None:
                            for n in range(colon_lo, colon_hi+1):
                                keywordArgList.append(
                                    args[:colon_arg] + [n] + args[colon_arg+1:])

                        elif args: # This distributes over the first index - is that
                                    # correct?
                                    # a(3,4) = 11*5 sets a(3,4) through a(13,4) = 5
                                    # instead of a(3,4) through a(3,14)
                                    # Hopefully nobody depends on this behavior
                            for n in range(args[0], args[0]+numVals):
                                keywordArgList.append([n] + args[1:])
                        else:
                            # hack for species eq
                            # TODO - do we need to dothis for more
                            # keywords which start at 0, like momentum_eq (?)
                            start = 0 if key == 'species_eq' else 1
                            for val in range(start, numVals+1):
                                keywordArgList.append([val]+args[1:])
                    else:
                        keywordArgList.append(args)

                    for keywordArgs, cleanVal in zip(keywordArgList, cleanVals):
                        yield (key, keywordArgs, cleanVal)

                else:
                    yield (key, args, cleanVals[0])

                if single_key:
                    break # no more keywords on this line
        else:
            yield (None, None, None)


    def parse_mfix_gui_comments(self, file):
        """read through the file looking for #!MFIX-GUI"""
        self.mfix_gui_comments.clear()
        key = None
        for i, line in enumerate(file):
            line = line.strip()
            if line.startswith('#!MFIX-GUI  '):  #continuation line
                if key:
                    self.mfix_gui_comments[key] += line[12:].rstrip()
            elif line.startswith('#!MFIX-GUI'):
                if '=' in line:
                    key, val = line[10:].split('=', 1)
                    key, val = key.strip(), val.strip()
                    self.mfix_gui_comments[key] = val
                    # For now, just ignore any other lines in this block  - it's just
                    # comments, and an experimental feature.  Don't want to create
                    # tight dependencies on GUI version - treat them like HTML tags,
                    # ignore the ones you can't handle

        # look for parameters
        if 'parameters' in self.mfix_gui_comments:
            self.parameters_from_str(self.mfix_gui_comments['parameters'])

    def parsemfixdat_inner(self, file):
        """This does the actual parsing."""
        self.comments.clear()
        self.keyword_dict.clear()
        self.init_data()
        self.dat_file_list = []
        self.thermo_data.clear()
        reactionSection = False
        desReactionSection = False
        thermoSection = False
        thermo_lines = [] # Temporary holder for thermo_data section
        reaction_lines = [] # reaction section
        des_reaction_lines = [] # DES reaction section

        # parse MFIX-GUI comments first
        self.parse_mfix_gui_comments(file)

        file.seek(0)

        for lineno, line in enumerate(file, 1):
            line0 = line
            line = line.strip()
            if line.startswith("#!MFIX-GUI"):
                # these should be already parsed
                continue
            if line.startswith("# CHEMICAL REACTION SECTION"):
                # This line will be replaced when we save reactions
                continue
            if '@(RXNS)' in line:
                if reaction_lines:
                    raise SyntaxError('multiple @(RXNS) sections')
                if reactionSection or desReactionSection:
                    raise SyntaxError('mismatched @(RXNS) statement')
                reactionSection = True
            elif '@(END)' in line:
                if not reactionSection:
                    raise SyntaxError('mismatched @(END) statement')
                reactionSection = False
            elif '@(DES_RXNS)' in line:
                if des_reaction_lines:
                    raise SyntaxError('multiple @(DES_RXNS) sections')
                if reactionSection or desReactionSection:
                    raise SyntaxError('mismatched @(DES_RXNS) statement')
                desReactionSection = True
            elif '@(DES_END)' in line:
                if not desReactionSection:
                    raise SyntaxError('mismatched @(DES_END) statement')
                desReactionSection = False
            elif 'thermo data' in line.lower():
                if thermoSection:
                    raise SyntaxError('multiple THERMO DATA sections')
                thermoSection = True
                # Don't save 'THERMO SECTION' line - we'll regenerate it.
            elif reactionSection:
                reaction_lines.append((lineno, line.strip()))
            elif desReactionSection:
                des_reaction_lines.append((lineno, line.strip()))
            elif thermoSection:
                thermo_lines.append(line0.rstrip()) # keep left padding
            elif not reactionSection and not thermoSection:
                equation_str = None
                # remove comments
                commentedline = ''
                if line.startswith('#') or line.startswith('!'):
                    commentedline = line
                    line = ''
                elif '#' in line or '!' in line:
                    line, keywordComment = re.split('[#!]', line, maxsplit=1)

                    # look for equation
                    if 'MFIX-GUI' in keywordComment and 'eq{' in keywordComment:
                        start = keywordComment.find('eq{')
                        equation_str = keywordComment[start+3:keywordComment.find('}', start)]
                        keywordComment = '' # clears the comment so that it is not saved again
                else:
                    keywordComment = ''

                # loop through all keywords in the line
                try:
                    for (key, args, value) in self.parseKeywordLine(line, equation_str):
                        if key is None:
                            self.dat_file_list.append(line+commentedline)
                        else:
                            try:
                                self.updateKeyword(key, value, args, keywordComment)
                            except ValueError:
                                # TODO:  allow user to fix value, like we do at mfix runtime
                                warnings.warn("Line %d: Cannot set %s=%s" % (lineno, format_key_with_args(key, args), value))
                except Exception as e:
                    traceback.print_exception(*sys.exc_info())
                    warnings.warn("Parse error: %s: line %d, %s" % (e, lineno, line))

        # turn THERMO DATA section into a dictionary
        if thermo_lines:
            species = None
            for line in thermo_lines:
                if not line.strip():
                    species = None
                    continue
                # Skip over comment block.
                #  (Should we just use read_burcat here? - comment is dropped)
                line = line.rstrip()
                if species is None and 79<=len(line)<=80 and line.endswith('1'):
                    species = line[:18].strip()
                    self.thermo_data[species] = []
                if species and line:
                    self.thermo_data[species].append(line)

        # parse reaction section
        if reaction_lines or des_reaction_lines:
            for lines in reaction_lines, des_reaction_lines:
                RP = ReactionParser()
                for (lineno, line) in lines:
                    try:
                        RP.parse(line)
                    except Exception as e:
                        warnings.warn("Parse error: %s: line %d, %s" % (e, lineno, line))
                self.reactions.update(RP.reactions)


    def updateKeyword(self, key, value, args=None,  keywordComment=None):
        """Update or add a keyword to the project.  Raises ValueError if there is a
        problem with the key or value.
        """
        if isinstance(args, int):
            args = [args]
        elif args is None:
            args = []

        key = key.lower()

        # special handling of x_min, x_max, etc.
        if key in ['x_min', 'x_max', 'y_min', 'y_max', 'z_min', 'z_max']:
            par_key = key.replace('_', '')
            PARAMETER_DICT[par_key] = value

        # check to see if the keyword already exists
        keyword = self.get(key, args)
        if keyword is not None:
            # if previous value is equation, update the parameter dict (why?)
            if isinstance(keyword.value, Equation) or isinstance(value, Equation):
                self.update_parameter_map(value, key, args)
            keyword.updateValue(value)
            if keywordComment is not None:
                keyword.comment = keywordComment
            return keyword

        # if equation, update the parameter dict
        if isinstance(value, Equation):
            self.update_parameter_map(value, key, args)

        keyword = None

        # Normalize some values
        if isinstance(value, str):
            v_upper = value.upper()
            if v_upper == 'USR_DRAG':
                value = 'USER_DRAG'

            if value != v_upper:
                for l in (TURBULENCE_MODELS, FRICTION_MODELS,
                          DRAG_TYPES, SUBGRID_TYPES, KT_TYPES,
                          RDF_TYPES, BLENDING_FUNCTIONS,
                          BC_TYPES, IS_TYPES,
                          PRECON_TYPES, SWEEP_TYPES,
                          DES_OUTPUT_TYPES): # How to keep in sync with constants.py?  Need "constants.all"
                    if v_upper in l:
                        value = v_upper
                        break

        if args:
            # Find condition keywords and separate
            if key.startswith('ic_'):
                cond = self.ics
            elif key.startswith('bc_') and not key.endswith('_q'): # q=quadric
                cond = self.bcs
                if key == 'bc_type':
                    # Normalize names NO_SLIP_WALL -> NSW, etc
                    value = value.upper()
                    value = BC_TYPE_DICT.get(value, value)
            elif key.startswith('ps_'):
                cond = self.pss
            elif key.startswith('is_'):
                if key == 'is_type':
                    # Normalize names
                    value = value.upper()
                    value = IS_TYPE_DICT.get(value, value)
                cond = self.iss
            elif key.startswith('vtk_') and 'vtk_var' not in key:
                if key == 'vtk_select_mode':
                    # Normalize
                    value = value.upper()
                cond = self.vtks
            else:
                cond = None
            # Save conditions
            if cond is not None:
                if len(args) == 1: # 1-dimensional
                    keyword = Keyword(key, value, args=args,
                                      comment=keywordComment)
                    cond[args[0]][key] = keyword
                # Gas Keys
                elif len(args) == 2 and key.endswith('_g'):
                    condItm = cond[args[0]]
                    if args[1] not in condItm.gasSpecies:
                        spec = condItm.gasSpecies.new(args[1])
                    else:
                        spec = condItm.gasSpecies[args[1]]
                    keyword = Keyword(key, value, args=args,
                                      comment=keywordComment)
                    spec[key] = keyword
                # Solid Keys
                elif key.endswith('_s'):
                    condItm = cond[args[0]]
                    if args[1] not in condItm.solids:
                        solid = condItm.solids.new(
                            args[1])
                    else:
                        solid = condItm.solids[args[1]]
                    if len(args) == 2:
                        keyword = Keyword(key, value, args=args,
                                          comment=keywordComment)
                        solid[key] = keyword
                    elif len(args) == 3:
                        if args[2] not in solid.species:
                            spec = solid.addSpecies(args[2])
                        else:
                            spec = solid.species[args[2]]
                        keyword = Keyword(key, value, args=args,
                                                comment=keywordComment)
                        spec[key] = keyword
                else:
                    # Create keyword objects for other keys not handled above,
                    # eg IC_THETA_M(IC, Phase),
                    # (see 'save everything else' comment below)
                    # TODO - is there more to do here?
                    keyword = Keyword(key, value, args=args,
                                      comment=keywordComment)


            # Solid Species
            elif key in ['species_s', 'species_alias_s', 'mw_s', 'd_p0',
                         'ro_s0', 'nmax_s', 'c_ps0', 'k_s0', 'x_s0', 'ro_xs0',
                         'solids_model', 'close_packed', ]:
                if args[0] not in self.solids:
                    solid = self.solids.new(args[0])
                else:
                    solid = self.solids[args[0]]
                if len(args) == 1:
                    keyword = Keyword(key, value, args=args,
                                      comment=keywordComment)
                    solid[key] = keyword
                else:
                    if args[1] not in solid.species:
                        spec = solid.addSpecies(args[1])
                    else:
                        spec = solid.species[args[1]]
                    keyword = Keyword(key, value, args=args,
                                      comment=keywordComment)
                    spec[key] = keyword
            # Gas Species
            elif key in ['species_g', 'species_alias_g', 'mw_g']:
                if args[0] not in self.gasSpecies:
                    spec = self.gasSpecies.new(args[0])
                else:
                    spec = self.gasSpecies[args[0]]
                keyword = Keyword(key, value, args=args,
                                  comment=keywordComment)
                spec[key] = keyword

            # variable grid
            elif key in ['cpx', 'ncx', 'erx', 'first_dx', 'last_dx', 'cpy',
                         'ncy', 'ery', 'first_dy', 'last_dy', 'cpz', 'ncz',
                         'erz', 'first_dz', 'last_dz']:
                if args[0] not in self.variablegrid:
                    variablegrid = self.variablegrid.new(args[0])
                else:
                    variablegrid = self.variablegrid[args[0]]
                keyword = Keyword(key, value, args=args,
                                  comment=keywordComment)
                variablegrid[key] = keyword
            # Save everything else
            else:
                keyword = Keyword(key, value, args=args,
                                  comment=keywordComment)
        else: # no args
            keyword = Keyword(key, value, args=None,
                              comment=keywordComment)

        # add keyword to other data structures
        if keyword is not None:
            self.put(key, keyword, args=args)
        else:
            raise ValueError(format_key_with_args(key, args))

        return keyword

    def keywordItems(self):
        for r in self.keyword_dict.values():
            for v in r.values():
                yield v


    def format_reaction(self, name):
        data = self.reactions[name]
        yield '%s {\n' % name
        # Split chem_eq if too long, replace empty string or None with 'NONE' (issues/499)
        chem_eq = data.get('chem_eq') or 'NONE'
        chem_eq, rest = self.break_string(chem_eq, 60)
        if not rest:
            yield '    chem_eq = "%s"\n' % chem_eq
        else:
            yield '    chem_eq = "%s" &\n' % chem_eq
            while rest:
                chem_eq, rest = self.break_string(rest, 60)
                yield '        "%s"%s\n' % (chem_eq, '&' if rest else '')

        for (key, val) in sorted(data.items()):
            if key in ('chem_eq', # handled above
                       'phases', 'reactants', 'products'): # keys we added
                continue
            if isinstance(val, dict):
                for (arg, subval) in sorted(val.items()):
                    yield '    %s(%s) = %s\n' % (key, arg, subval)
            else:
                yield '    %s = %s\n' % (key, val)
        yield '}\n'


    def break_string(self, s, w):
        if len(s) <= w:
            return (s, '')
        else:
            while w > 0 and s[w] != ' ':
                w -= 1
            if w == 0: # no space found, can't break
                return  (s, '')
            s, rest = s[:w], s[w:]
            if len(rest) <= 6: # Prefer a slightly long line to a 'dangler'
                return (s+rest, '')
            else:
                return (s, rest)

    def to_string(self):
        for line in self.dat_file_list:
            if hasattr(line, 'line'):
                yield line.line() + '\n'
            else:
                yield line + '\n'

        # Save chemistry
        rxns = []
        des_rxns = []
        # defer the decision whether a reaction is DES or not until as late as possible
        #  since solids models can change
        for (name, reaction) in self.reactions.items():
            if reaction.get('chem_eq') is None: # Don't save incompletely-defined reactions
                continue
            des = any(self.get('solids_model', args=[p]) in ('DEM', 'PIC')
                      for p in reaction.get('phases', []))
            if des:
                des_rxns.append(name)
            else:
                rxns.append(name)

        if rxns or des_rxns:
            yield '\n'
            yield '# CHEMICAL REACTION SECTION\n'
        if rxns:
            yield '@(RXNS)\n'
            for name in rxns:
                for line in self.format_reaction(name):
                    yield line
            yield '@(END)\n'
            yield '\n'
        if des_rxns:
            yield '\n'
            yield '@(DES_RXNS)\n'
            for name in des_rxns:
                for line in self.format_reaction(name):
                    yield line
            yield '@(DES_END)\n'
            yield '\n'

        if self.mfix_gui_comments: # Special comment block to hold non-keyword gui params
            yield '\n'
            yield '#!MFIX-GUI SECTION\n'

            for (key, val) in self.mfix_gui_comments.items():
                sval = str(val)
                keylen = len(key)
                # Make lines no longer than 80 chars, including #!MFIX-GUI prefix (11 chars)
                sval, rest = self.break_string(sval, 66-keylen) # 3 spaces for ' = ',
                yield '#!MFIX-GUI %s = %s\n' % (key, sval)
                while rest: # continuation lines
                    sval, rest = self.break_string(rest, 68) # 1 space for continuation
                    yield '#!MFIX-GUI  %s\n' % sval


        if self.thermo_data:
            yield '\n'
            yield 'THERMO DATA\n'
            for key in sorted(self.thermo_data.keys()):
                for line in self.thermo_data[key]:
                    yield line + '\n'
                yield '\n' # blank line

    def writeDatFile(self, fname):
        """ Write the project to specified text file"""

        # save parameters
        self.mfix_gui_comments['parameters'] = self.parameters_to_str()

        last_line = None
        with open(fname, 'w', encoding='utf-8', errors='replace') as dat_file:
            for line in self.to_string():
                if line == last_line == '\n': # Avoid multiple blank lines
                    continue
                last_line = line
                dat_file.write(line)

    def reset(self):
        self.dat_file = None
        self.dat_file_list = []
        self.keyword_dict.clear()
        self.comments.clear()
        self.thermo_data.clear()
        self.mfix_gui_comments.clear()
        self.parameter_key_map = {}
        self.reactions.clear()

        for name in dir(self):
            attr = getattr(self, name)
            if isinstance(attr, Collection):
                # Should be a 'reset' or 'clear' method for collections
                Collection.__init__(attr)

    def __deepcopy__(self, memo):
        # This is not efficient.  Remove?  (Only used in nodeworks widget)
        return Project(''.join(self.to_string()))

    def parameters_from_str(self, string):
        """load parameter data from a saved string"""
        loaded_data = ExtendedJSON.loads(string)

        if 'order' not in loaded_data:
            return

        data = OrderedDict()
        for par in loaded_data['order']:
            data[par] = loaded_data['parameters'][par]

        PARAMETER_DICT.update(data)
        for key in set(PARAMETER_DICT.keys()) - set(list(data.keys()) + SPECIAL_PARAMETERS + list(CONVERSION_TO_METERS.keys())):
            PARAMETER_DICT.pop(key)

    def parameters_to_str(self):
        """convert parameter data to a string for saving"""
        data = {
            'order': list(PARAMETER_DICT.keys()),
            'parameters': PARAMETER_DICT
        }
        return ExtendedJSON.dumps(data)

    def update_parameter_map(self, new_value, key, args):
        """update the dependency graph of parameters and keywords"""

        key_args = format_key_with_args(key, args)

        # new params
        new_params = new_value.get_used_parameters() if isinstance(new_value, Equation) else []

        # old params
        old_value = self.get_value(key, args=args)
        old_params = old_value.get_used_parameters() if isinstance(old_value, Equation) else []

        to_add = set(new_params)-set(old_params)
        for param in to_add:
            if param not in self.parameter_key_map:
                self.parameter_key_map[param] = set()
            self.parameter_key_map[param].add(key_args)

        to_remove = set(old_params)-set(new_params)
        for param in to_remove:
            self.parameter_key_map[param].remove(key_args)
            if not self.parameter_key_map[param]:
                self.parameter_key_map.pop(param)

def main():
    # Self-test.
    p = Project()
    inputs = ['key = @(2 * 10)',
              'key = 123',
              'key = "456"',
              'key = "QUOTED STRING"',
              'key = "STRING1" "STRING2"',
              'key = "STRING1", "STRING2"',
              'key = 11 12',
              'key = 101, 202']

    outputs = [
        [('key', [], Equation('2*10'))],
        [('key', [], 123)],
        [('key', [], '456')],
        [('key', [], 'QUOTED STRING')],
        [('key', [1], 'STRING1'), ('key', [2], 'STRING2')],
        [('key', [1], 'STRING1'), ('key', [2], 'STRING2')],
        [('key', [1], 11), ('key', [2], 12)],
        [('key', [1], 101), ('key', [2], 202)]]

    for (line, expected) in zip(inputs, outputs):
        result = list(p.parseKeywordLine(line))
        print("Input:", line)
        print("Output", result)
        assert (result==expected)


if __name__ == '__main__':
    main()
