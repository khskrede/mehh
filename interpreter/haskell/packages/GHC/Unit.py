

#TODO

import haskell.haskell as hh

x = hh.Var("()")
Z0T = hh.constr(x)

# Unit type: ()
#class Z0T(hh.Value):
#    _immutable_fields_ = ["value"]

#    def __init__(self):
#        self.value = ()

    #def match(self, other, subst):
    #    value = other.getvalue()
    #    if value:
    #        assert isinstance(value, Integer)
    #        if self.value == value.value:
    #            return DEFINITE_MATCH
    #        return NO_MATCH
    #    return NEEDS_HNF

    # def __eq__(self, other):
    #    return (isinstance(other, Z0T) and self.value == other.value)

    # def __ne__(self, other):
    #    return not (self == other)

#    def tostr(self):
#        return str(self.value)

