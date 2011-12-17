

import haskell.haskell as hh



class zm(hh.Value):
    _immutable_fields_ = ["value"]

    def __init__(self, integer):
        self.value = integer

    def match(self, other, subst):
        value = other.getvalue()
        if value:
            assert isinstance(value, Integer)
            if self.value == value.value:
                return DEFINITE_MATCH
            return NO_MATCH
        return NEEDS_HNF

    def __eq__(self, other):
        return (isinstance(other, Int) and self.value == other.value)

    def __ne__(self, other):
        return not (self == other)

    def tostr(self):
        return str(self.value)



# zt = *

class zt(hh.Value):
    _immutable_fields_ = ["value"]

    def __init__(self, integer):
        self.value = integer

    def match(self, other, subst):
        value = other.getvalue()
        if value:
            assert isinstance(value, Integer)
            if self.value == value.value:
                return DEFINITE_MATCH
            return NO_MATCH
        return NEEDS_HNF

    def __eq__(self, other):
        return (isinstance(other, Int) and self.value == other.value)

    def __ne__(self, other):
        return not (self == other)

    def tostr(self):
        return str(self.value)




class zdfNumInt(hh.Value):
    _immutable_fields_ = ["value"]

    def __init__(self, integer):
        self.value = integer

    def match(self, other, subst):
        value = other.getvalue()
        if value:
            assert isinstance(value, Integer)
            if self.value == value.value:
                return DEFINITE_MATCH
            return NO_MATCH
        return NEEDS_HNF

    def __eq__(self, other):
        return (isinstance(other, Int) and self.value == other.value)

    def __ne__(self, other):
        return not (self == other)

    def tostr(self):
        return str(self.value)





