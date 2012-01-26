
import haskell as h



@h.expose_primitive(2)
def ZLzmzgZR(args):
    a = args[0]
    b = args[1]
    return constr("->", a, b)


#-----------------------
# Char#
#-----------------------


class Charzh(h.Value):
    _immutable_fields_ = ["value"]

    def __init__(self, char):
        self.char = char

@h.expose_primitive(2)
def gtCharzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Charzh(a0.value > a1.value)

@h.expose_primitive(2)
def geCharzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Charzh(a0.value >= a1.value)

# TODO: Complete Char# implementation




#------------------------
# Int#
#------------------------

class Intzh(h.Value):
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

@h.expose_primitive(2)
def zpzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value + a1.value)

# (-#)

@h.expose_primitive(2)
def zmzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value - a1.value)

# (*#)

@h.expose_primitive(2)
def ztzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value * a1.value)

# mulIntMayOflo#
def mulIntMayOflozh(args):
    raise NotImplementedError


# Concurrency primitives

@h.expose_primitive(1)
def Statezh(args):
    return const("State#", args[0])

@h.expose_primitive(0)
def RealWorld(args):
    return const("RealWorld")


# Tuples

@h.expose_primitive(0)
def Z0T(args):
    return const("()")

@h.expose_primitive(2)
def Z2T(args):
    return const("()", args[0], args[1])


# Unboxed tuple


@h.expose_primitive(0)
def Z0H(args):
    return const("()")

@h.expose_primitive(2)
def Z2H(args):
    return const("()", args[0], args[1])

