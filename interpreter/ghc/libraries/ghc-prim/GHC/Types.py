

# TODO!

import haskell.haskell as hh

import Prim as P



#------------------
# BLEH
#------------------




# Data Char


@hh.expose_primitive(1)
def Czh(args):
    a0 = args
    return hh.constr("C#", a0)


@hh.expose_primitive(1)
def Char(a):
    return Czh(P.Charzh(a))

# Data Int 

@hh.expose_primitive(1)
def Izh(args):
    a0 = args
    return hh.constr("I#", a0)


@hh.expose_primitive(1)
def Int(a):
    return Izh(P.Intzh(a))


# Data Float




# Data Double




# Newtype IO



@hh.expose_primitive(1)
def IO(a):
    print "MEH!"
    return hh.constr("IO", a[0])


