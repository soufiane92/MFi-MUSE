# Class mixin to support comparison

# Idea found at
#https://regebro.wordpress.com/2010/12/13/python-implementing-rich-comparison-the-correct-way/
# modified slightly to allow 'other' not to have _cmpkey method (to allow comparison with constants)


class Comparable(object):
    def _compare(self, other, method):
        try:
            return method(self._cmpkey(),
                          other._cmpkey()
                          if hasattr(other, '_cmpkey') else other)
        except TypeError:
            return NotImplemented

    def __lt__(self, other):
        return self._compare(other, lambda s, o: s < o)

    def __le__(self, other):
        return self._compare(other, lambda s, o: s <= o)

    def __eq__(self, other):
        return self._compare(other, lambda s, o: s == o)

    def __ge__(self, other):
        return self._compare(other, lambda s, o: s >= o)

    def __gt__(self, other):
        return self._compare(other, lambda s, o: s > o)

    def __ne__(self, other):
        return self._compare(other, lambda s, o: s != o)
