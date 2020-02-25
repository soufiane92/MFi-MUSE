#!/usr/bin/env python

import sys

NoneType = type(None)
EllipsisType = type(...)

dtypes = {}
defaults = {}
key_indices = {}

type_map = {'C': str, 'I': int, 'DP': float, 'L':bool}

debug = False
if debug:
    dprint = print
else:
    dprint = lambda *args: None

UPCASE_KEYS = False  # MFiX keys can be lower or upper case
def fixcase(x):
    return x.upper() if UPCASE_KEYS else x.lower()


try:
    import mfixgui.namelistparser as NP
    dprint("fetching keyword_doc")
    kd = NP.getKeywordDoc()
    for k, v in kd.items():
        key = fixcase(k)
        dtypes[key] = type_map[v['dtype']]
        default = v.get('initpython')
        args = v.get('args') # May be an empty dict
        if args:
            key_indices[key] = args
        if default is not None:
            defaults[key] = default

except ImportError:
    # For demo purposes
    dprint("Cannot import mfixgui, loading static data")
    for line in open('key_data.txt', encoding='utf-8', errors='replace').read().splitlines():
        row = line.split()
        if len(row) == 2:
            row.append('')
        key, dtype, default = row
        key = fixcase(key)
        dtype = eval(dtype)
        default = None if default == 'None' else dtype(default)
        dtypes[key] = dtype
        if default is not None:
            defaults[key] = default

keys = set(dtypes.keys())

def typename(t):
    # <class 'int'> -> int
    t = str(t)
    if "'" not in t:
        return t
    return t.split("'")[1]


def check_type(key, val):
    # Raise an exception if val is not valid type
    # Allow promoting int to float
    expected = dtypes[key]
    got = type(val)
    if expected is float and got is int:
        return
    if got not in (NoneType, expected):
        raise TypeError('key %s: expected %s, got %s' %
                        (key, typename(expected), typename(got)))


def check_indices(key, indices):
    # Raise an exception if indices are out of range, or non-int type
    expected = key_indices.get(key, [])
    if len(indices) != len(expected):
        raise IndexError("key %s: expected %s indices, got %s" %
                         (key, len(expected), len(indices)))

    for (i,index) in enumerate(indices, 1):
        if not isinstance(index, (int, EllipsisType)):
            raise TypeError("key %s: indices must be integer, not %s" %
                            (key, typename(type(index))))
        if index is ...:
            return

        mn = expected[i].get('min')
        mx = expected[i].get('max')
        if (isinstance(mn, int) and index < mn or
            isinstance(mx, int) and index > mx):
            raise ValueError("key %s index %s (%s) out of range" %
                             (key, i, index))


def key_match(key1, key2):
    return (len(key1)==len(key2)
            and all(k1==k2 or ... in (k1, k2)
                    for (k1,k2) in zip(key1, key2)))


def run_hooks(key, val):
    print("update", key, val)


class Undefined(object):
    """Value placeholder for MFIX keys which are [UNDEFINED],
    which still may have default values"""
    # Undefined keys only exist within a Namelist.  Some
    #  operations on the Undefined object trigger an action
    #  on the containing Namelist

    def __init__(self, namelist, key):
        self.namelist = namelist
        self.key = fixcase(key)


    def __str__(self):
        return 'Undefined'
    __repr__ = __str__


    def __setitem__(self, indices, val):
        # Set n[key][indices] = val
        dprint("Undefined.__setitem__(%s)=%s" % (repr(indices), repr(val)))
        check_type(self.key, val)
        if isinstance(indices, int):
            indices = (indices,)
        check_indices(self.key, indices)
        key = (self.key,) + indices
        if ... in indices:
            for (k, v) in list(self.namelist.__data__.items()):
                if key_match(k, key):
                    if val is None:
                        del(self.namelist.__data__[k])
                        run_hooks(k, val)
                    else:
                        prev = self.namelist.__data__.get(k)
                        if prev == val:
                            continue
                        self.namelist.__data__[k] = val
                        run_hooks(k, val)
            return

        if indices and val is None:
            del self.namelist.__data__[key]
        else:
            prev = self.namelist.__data__.get(key)
            if prev == val: # Idepotence
                return
            self.namelist.__data__[key] = (Undefined(self.namelist, self.key)
                                           if val is None
                                           else val)
        run_hooks(key, val)


    def __getitem__(self, indices):
        # Fetch keyword value as N['key'][indices]
        dprint("Undefined.__getitem__(%s)" % repr(indices))
        if isinstance(indices, int):
            indices = (indices,)
        check_indices(self.key, indices)
        key = (self.key,) + indices
        r = self.namelist.__data__.get(key, self)
        if defined(r):
            return r
        expected = key_indices.get(self.key,[])
        if len(indices) == len(expected):
            r = defaults.get(key[0], r) # Return default, only if fully qualified
        return r


    def __delitem__(self, indices):
        self.__setitem__(indices, None)


    def __eq__(self, other):
        return isinstance(other, (Undefined, NoneType))


def undefined(val):
    return isinstance(val, (Undefined, NoneType))

def defined(val):
    return not isinstance(val, (Undefined, NoneType))

def format_key(key):
    if isinstance(key, tuple):
        if len(key) > 1:
            return '%s[%s]'%(key[0], ','.join(map(str,key[1:])))
        return key[0]
    return key


def format_val(val):
    if isinstance(val, bool):
        return '.T.' if val else '.F.'
    if isinstance(val, str):
        return repr(val)
    if isinstance(val, float):
        return round(val, 12)
    return val


class Namelist(object):

    def __init__(self):
        self.__data__ = {(key,): Undefined(self, key) for key in keys}


    def __iter__(self):
        return iter(self.__data__)


    def __contains__(self, name):
        key = name if isinstance(name, tuple) else (name,)
        return key in self.__data__


    def __getitem__(self, name):
        # Fetch keyword value via N['key'] or N['key', args]
        dprint("Namelist.__getitem__(%s)" % repr(name))
        if isinstance(name, str):
            key = (fixcase(name),)
        else:
            key = (fixcase(name[0]),) + (name[1:])
        r = self.__data__.get(key)
        if r is None:
            raise KeyError(name)
        return r


    def __setitem__(self, item, val):
        # Set keyword value via N['key'] = val or N['key', args] = val
        dprint("Namelist.__setitem__(%s) = %s" % (repr(item), repr(val)))
        if isinstance(item, tuple):
            key = fixcase(item[0])
            indices = item[1:]
        else:
            key = fixcase(item)
            indices = ()
        check_type(key, val)
        check_indices(key, indices)
        # TODO ellipsis
        key = (key,) + indices
        prev = self.__data__.get(key)
        if prev == val: # Idempotence
            return
        self.__data__[key] = Undefined(self, key[0]) if val is None else val
        run_hooks(key, val)


    def __delitem__(self, item):
        print("Namelist.delitem(%s)" % (item,))
        if isinstance(item, tuple):
            base_key = fixcase(item[0])
            indices = item[1:]
        else:
            base_key = fixcase(item)
            indices = ()

        check_indices(key, indices)
        # TODO ellipsis
        deleted = False
        for (k,v) in list(self.__data__.items()): # Modifying dict
            if k[:1+len(indices)] == (base_key,) + indices:
                if defined(v):
                    deleted = True
                    self.__setitem__(k, None)

        #if not deleted:
        #    raise KeyError(item)

    __delattr__ = __delitem__


    def __getattr__(self, attr):
        # Get keyword value as N.key
        dprint("Namelist.__getattr__(%s)" % repr(attr))
        key = fixcase(attr)
        if key not in keys:
            raise NameError("%s: no such key" % attr)
        key = (key,)
        val = self.__data__[key]
        if undefined(val) and not key_indices.get(key[0]):
            return defaults.get(key[0], val)
        return val


    def __setattr__(self, attr, val):
        # Set keyword value via N.key = val
        if attr.startswith('__'):
            object.__setattr__(self, attr, val)
            return
        dprint("Namelist.__setattr__(%s)=%s" % (repr(attr), repr(val)))
        key = fixcase(attr)
        if key not in keys:
            raise NameError("%s: no such key" % attr)
        expected = key_indices.get(key)
        if expected: # TODO allow bulk setting from array
            raise IndexError("key %s: expected %s indices, got 0" %
                             (attr, len(expected)))
        key = (key,)
        prev = self.__data__.get(key)
        if prev == val: # Idempotence
            return
        check_type(key[0], val)
        self.__data__[key] = Undefined(self, key[0]) if val is None else val
        run_hooks(key, val)


    def __dir__(self):
        return keys


    def indices(self, key):
        r = []
        if isinstance(key, str):
            key = (fixcase(key),)
        else:
            key = (fixcase(key[0]), key[1:])
        key_len = len(key)
        for (k,v) in self.__data__.items():
            if k[:key_len] == key and not undefined(v):
                r.append(k[key_len:])
        return r


    def dump(self, file=sys.stdout, write_defaults=False):
        for key in sorted(self.__data__.keys()):
            val = self.__data__[key]
            if undefined(val):
                val = defaults.get(key[0]) if write_defaults else None
                if val is None:
                    continue
            print(format_key(key), '=', format_val(val), file=file)
        return True


if (__name__ == '__main__'):
    n = Namelist()

    for line in """
    n.bc_x_s[1,2,3] = False    # Wrong type
    n.bc_x_s[1000, 2, 3] = 4   # Index out of bounds
    n.bc_x_s[0, 2, 3] = 4      # Index out of bounds
    n.bc_x_s[1, 2, 3, 4] = 5   # Too many indices
    n.bc_x_s[1, 2, 3] = 4      # Should succeed
    n.bc_x_s[1, 2, 3] = 4      # Idempotent (no update)
    n.bc_x_s[1, 2, 3]          # Report value
    n['bc_x_s', 1, 2, 3]       # Alternate syntax
    n.nmax_s[1]                # No default value, undefined
    undefined(n.nmax_s[1])     # Test for undefinedness
    n.nmax_s[1]= 5             # Set value
    defined(n.nmax_s[1])       # Test for definition
    n.energy_eq                # Defaults to 'true'
    n.energy_eq = False        # Change it
    n.energy_eq = False        # Keys are case-insensitive (no update)
    n.ENERGY_EQ = False        # Keys are case-insensitive (no update)
    n.no_such_key = 55         # Invalid key (different from Undefined)
    n.no_such_key              # Invalid key
    n['solids_model'][3] = 'TFM' # Another alternative syntax
    n.solids_model[4] = 'PIC'  # Set another one
    n.dump()                   # Save to file or string
    n.bc_x_s[1,3,5] = 33       # Set more array elements
    n.indices('bc_x_s')        # Which indices are set for key?
    n.bc_x_s[10,10,20] = 2020  # Set another one
    n.bc_x_s[10,15,20] = 2030  # Index out of range
    n.indices('bc_x_s')        # Which indices are set for key?
    [(i,n.bc_x_s[i]) for i in n.indices('bc_x_s')] # Show indices and values
    del n.bc_x_s[10,10,20]     # Clear one element
    n.indices('bc_x_s')        # Show the indices now
    'energy_eq' in n           # Test for membership
    ('bc_x_s',1,3,5) in n      # Another membership test
    'fred' in n # Bogus key""".splitlines():
        line = line.strip()
        if not line:
            continue
        print('>>> ', line, sep='')
        if 'del' in line or '=' in line:
            try:
                exec(line)
            except Exception as e:
                print(e)
        else:
            try:
                r = eval(line)
            except Exception as e:
                r = e
            print(r)
