from haskell import *

class ForwardReference(object):
    def become(self, x):
        self.__class__ = x.__class__
        self.__dict__.update(x.__dict__)
    def enumerate(self, subst):
        return self

def enum(rule, subst):
    patterns, body = rule
    patterns = [p.enumerate(subst) for p in patterns]
    body = body.enumerate(subst)
    return patterns, body

def function(name, rules):
    subst = {}
    rules = [enum(rule, subst) for rule in rules]
    return Function(name, rules, len(subst))

def app(func, *args):
    return Cell(Application(func, list(args)))
