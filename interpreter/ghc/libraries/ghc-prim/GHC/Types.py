

# TODO!

import haskell.haskell as hh










#------------------
# BLEH
#------------------


x = hh.Var("x")
IO = hh.constr("IO", x)



class Int(hh.Value):
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

# Izh = I#

#Izh = hh.constr("Izh", hh.Var("x"))


#@hh.expose_primitive(1)
#def Izh(args):
#    a0 = args[0]
#    return Intzh(a0.value)

class Izh(hh.Value):
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


