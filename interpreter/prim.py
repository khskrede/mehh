
import haskell as h
import module as m

# Create Prim module
prim = m.coremod("ghc-prim:GHC.Prim")

# Define primitive functions
# ____________________________________

# ("->", a, b)
@h.expose_primitive(2)
def ZLzmzgZR(args):
    a = args[0]
    b = args[1]
    return constr("->", a, b)

prim.qtycons["GHC.Prim.ZLzmzgZR"] = ZLzmzgZR

# Char#
class Charzh(h.Value):
    _immutable_fields_ = ["value"]

    def __init__(self, char):
        self.char = char

# TODO: Where do values belong????

@h.expose_primitive(2)
def gtCharzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Charzh(a0.value > a1.value)

prim.qvars["GHC.Prim.gtCharzh"] = gtCharzh

@h.expose_primitive(2)
def geCharzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Charzh(a0.value >= a1.value)

prim.qvars["GHC.Prim.geCharzh"] = geCharzh

# Int#

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

prim.qvars["GHC.Prim.zpzh"] = zpzh

# (-#)

@h.expose_primitive(2)
def zmzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value - a1.value)

prim.qvars["GHC.Prim.zmzh"] = zmzh

# (*#)

@h.expose_primitive(2)
def ztzh(args):
    a0, a1 = args
    assert isinstance(a0, Charzh)
    assert isinstance(a1, Charzh)
    return Intzh(a0.value * a1.value)

prim.qvars["GHC.Prim.ztzh"] = ztzh

# mulIntMayOflo#
def mulIntMayOflozh(args):
    raise NotImplementedError


# Concurrency primitives

@h.expose_primitive(1)
def Statezh(args):
    return const("State#", args[0])

prim.qtycons["GHC.Prim.Statezh"] = Statezh

@h.expose_primitive(0)
def RealWorld(args):
    return const("RealWorld")

prim.qtycons["GHC.Prim.RealWorld"] = RealWorld

# Tuples

@h.expose_primitive(0)
def Z0T(args):
    return const("()")

prim.qtycons["GHC.Prim.Z0T"] = Z0T

@h.expose_primitive(2)
def Z2T(args):
    return const("()", args[0], args[1])

prim.qtycons["GHC.Prim.Z2T"] = Z2T

# Unboxed tuple


@h.expose_primitive(0)
def Z0H(args):
    return const("()")

prim.qtycons["GHC.Prim.Z0H"] = Z0H

@h.expose_primitive(2)
def Z2H(args):
    return const("()", args[0], args[1])

prim.qtycons["GHC.Prim.Z2H"] = Z2H

