

from haskell import haskell as hh

#a = hh.Var("a")
#b = hh.Var("b")
#ZLzmzgZR = hh.constr("->", a, b)


def ZLzmzgZR(a, b):
    return hh.constr("->", a, b)


#-----------------------
# Char#
#-----------------------


class Charzh(hh.Value):
    _immutable_fields_ = ["value"]

    def __init__(self, char):
        self.char = char

@hh.expose_primitive(2)
def gtCharzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Charzh(a0.value > a1.value)

@hh.expose_primitive(2)
def geCharzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Charzh(a0.value >= a1.value)

# TODO: Complete Char# implementation




#------------------------
# Int#
#------------------------

class Intzh(hh.Value):
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

#  (+#)

@hh.expose_primitive(2)
def zpzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value + a1.value)

# (-#)

@hh.expose_primitive(2)
def zmzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value - a1.value)

# (*#)

@hh.expose_primitive(2)
def ztzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value * a1.value)

# mulIntMayOflo#
def mulIntMayOflozh(args):
    raise NotImplementedError


# Concurrency primitives

@hh.expose_primitive(1)
def Statezh(args):
    return hh.const("State#", args[0])

@hh.expose_primitive(0)
def RealWorld(args):
    return hh.const("RealWorld")


# Tuples

@hh.expose_primitive(0)
def Z0T(args):
    return hh.const("()")

@hh.expose_primitive(2)
def Z2T(args):
    return hh.const("()", args[0], args[1])


# Unboxed tuple


@hh.expose_primitive(0)
def Z0H(args):
    return hh.const("()")

@hh.expose_primitive(2)
def Z2H(args):
    return hh.const("()", args[0], args[1])

