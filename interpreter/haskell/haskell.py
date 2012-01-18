from pypy.rlib import jit, unroll

class Symbol(object):
    """ A cached symbol that can be compared by identity (which is not true for
    strings). """
    def __init__(self, str):
        self.str = str

    all_symbols = {}

    @staticmethod
    def get_symbol(str):
        if str in Symbol.all_symbols:
            return Symbol.all_symbols[str]
        result = Symbol(str)
        Symbol.all_symbols[str] = result
        return result

    def tostr(self):
        return self.str

class HaskellObject(object):
    """ Base class for all objects that the interpreter handles. """

    _attrs_ = []
    def step(self, todo):
        """ Perform and interpretation step where self should move in the
        direction of being more evaluated. """
        raise NotImplementedError

    def getvalue(self):
        """ Dereference self to be a value. Can return None if self is not
        evaluated."""
        return None

    def substitute(self, subst):
        """ Perform a substitution. """
        raise NotImplementedError

    def enumerate_head(self, subst):
        """ Enumerate the variables in self. """
        raise NotImplementedError

    def replace_vars(self, subst, occurrences):
        """ Replace varibles in self with what is found in subst."""
        raise NotImplementedError

    def __str__(self):
        return self.tostr()

class Value(HaskellObject):
    """ Base class for evaluated values (i.e. already in head-normal form). """

    def step(self, todo):
        """ self is already a value. Continue with the todo stack."""
        return todo.step(self)

    def getvalue(self):
        """ self is a value, just return it. """
        return self

    def substitute(self, subst):
        return self

    def enumerate_head(self, subst):
        return self

    def replace_vars(self, subst, occurrences):
        return self

NO_MATCH = 0
DEFINITE_MATCH = 1
NEEDS_HNF = 2

class Constructor(Value):
    """ A constructor. This is an abstract base class, there are
    subclasses generated below for various numbers of arguments."""

    _immutable_fields_ = ["symbol"]
    def __init__(self, symbol):
        assert isinstance(symbol, Symbol)
        self.symbol = symbol

    @jit.unroll_safe
    def match(self, other, subst):
        value = other.getvalue()
        if value is None:
            return NEEDS_HNF
        assert isinstance(value, Constructor)
        if self.symbol is not value.symbol:
            return NO_MATCH
        for i in range(self.numargs()):
            arg1 = self.getarg(i)
            assert isinstance(arg1,NumberedVar)
            arg2 = value.getarg(i)
            res = arg1.match(arg2, subst)
            assert res == DEFINITE_MATCH
        return DEFINITE_MATCH

    @jit.unroll_safe
    def substitute(self, subst):
        args = [None] * self.numargs()
        for i in range(self.numargs()):
            args[i] = self.getarg(i).substitute(subst)
        return make_constructor(self.symbol, args)

    def enumerate_head(self, subst):
        args = [None] * self.numargs()
        for i in range(self.numargs()):
            args[i] = self.getarg(i).enumerate_head(subst)
        return make_constructor(self.symbol, args)
    
    def replace_vars(self, subst, occurrences):
        args = [None] * self.numargs()
        for i in range(self.numargs()):
            args[i] = self.getarg(i).replace_vars(subst, occurrences)
        return make_constructor(self.symbol, args)

    def __eq__(self, other):
        return (isinstance(other, Constructor) and
                self.symbol is other.symbol and
                self.getargs() == other.getargs())

    def __ne__(self, other):
        return not (self == other)

    def tostr(self):
        return "(%s %s)" % (self.symbol.tostr(), 
                            " ".join([arg.tostr() for arg in self.getargs()]))

    @jit.unroll_safe
    def getargs(self):
        args = [None] * self.numargs()
        for i in range(self.numargs()):
            args[i] = self.getarg(i)
        return args

def make_arg_subclass(n, base):

    rangen = unroll.unrolling_iterable(range(n))

    class cls(base):
        _immutable_fields_ = ["arg%s" % i for i in rangen]

        def __init__(self, function, args):
            base.__init__(self, function)
            for i in rangen:
                setattr(self, "arg%s" % i, args[i])
            assert len(args) == n

        def numargs(self):
            return n

        def getarg(self, i):
            assert i < n
            for j in rangen:
                if i == j:
                    return getattr(self, "arg%s" % j)
            assert 0

    cls.__name__ = "%s%s" % (base.__name__, n)
    return cls

specialized_constr_classes = [make_arg_subclass(i, Constructor) for i in range(11)]

class ConstructorN(Constructor):
    _immutable_fields_ = ["args[*]"]

    def __init__(self, function, args):
        Constructor.__init__(self, function)
        self.args = args

    def numargs(self):
        return len(self.args)

    def getarg(self, i):
        return self.args[i]

def make_constructor(function, args):
    if len(args) < len(specialized_app_classes):
        cls = specialized_constr_classes[len(args)]
    else:
        cls = ConstructorN
    return cls(function, args)

def constr(name, *args):
    assert all([isinstance(arg, HaskellObject) for arg in args])
    return make_constructor(Symbol.get_symbol(name), list(args))

class Integer(Value):
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
        return (isinstance(other, Integer) and self.value == other.value)

    def __ne__(self, other):
        return not (self == other)

    def tostr(self):
        return str(self.value)


class CString(Value):
    _immutable_fields_ = ["value"]

    def __init__(self, string):
        self.value = string

    #def match(self, other, subst):
    #    value = other.getvalue()
    #    if value:
    #        assert isinstance(value, Integer)
    #        if self.value == value.value:
    #            return DEFINITE_MATCH
    #        return NO_MATCH
    #    return NEEDS_HNF

    def __eq__(self, other):
        return (isinstance(other, CString) and self.value == other.value)

    def __ne__(self, other):
        return not (self == other)

    def tostr(self):
        return str(self.value)





class AbstractFunction(Value):
    _immutable_fields_ = []

    def __init__(self):
        pass

    def apply(self, application, todo):
        raise NotImplementedError

class Function(AbstractFunction):
    """ A user-defined function, i.e. written in Haskell. """
    _immutable_fields_ = ["name", "rules[*]"]

    def __init__(self, name, rules):
        AbstractFunction.__init__(self)
        self.name = name
        self.rules = rules
 
    def tostr(self):
        return self.name

    @jit.unroll_safe
    def apply(self, application, todo):
        for i in range(len(self.rules)):
            rule = self.rules[i]
            subst, todo = rule.apply(application, todo)
            if subst is not None:
                return subst, todo
        assert 0

def enum(rule, subst):
    patterns, body = rule
    patterns = [p.enumerate_head(subst) for p in patterns]
    body = body.enumerate_head(subst)
    return patterns, body

def function(name, rules, recursive=False):
    rules = [Rule(pattern, body, recursive) for pattern, body in rules]
    return Function(name, rules)

class Rule(object):
    """ One rule of a user-defined function. """
    _immutable_fields_ = ["patterns[*]","body","numvars","occurrences[*]",
                          "recursive"]

    def __init__(self, patterns, body, recursive):
        subst = {}
        self.patterns = [pattern.enumerate_head(subst) for pattern in patterns]
        occurrences = [0] * len(subst)
        self.body = body.replace_vars(subst, occurrences)
        self.recursive = recursive
        self.numvars = len(subst)
        self.occurrences = occurrences

    @jit.unroll_safe
    def apply(self, application, todo):
        assert len(self.patterns) == application.numargs()
        subst = [None] * self.numvars
        for i in range(application.numargs()):
            arg = application.getarg(i)
            match = self.patterns[i].match(arg, subst)
            if match == NO_MATCH:
                return None, todo
            elif match == NEEDS_HNF:
                return application.eval_after(arg, i, todo)
            else:
                assert match == DEFINITE_MATCH

        # introduce thunks necessary for sharing
        for i in range(len(subst)):
            val = subst[i].getvalue()
            if val is not None:
                subst[i] = val
            elif (isinstance(subst[i], Application) and
                  self.occurrences[i] > 1):
                subst[i] = Thunk(subst[i])
        res = Substitution(self.body, subst, self.recursive)

        return res, todo

class Substitution(HaskellObject):
    """ The body of a function with numbered variables substituted by values.
    """
    _immutable_fields_ = ["rhs", "subst", "recursive"]

    def __init__(self, rhs, subst, recursive):
        self.rhs = rhs
        self.subst = subst
        self.recursive = recursive

    def apply(self):
        rhs = jit.hint(self.rhs, promote=True)
        return rhs.substitute(self.subst)


class PrimFunction(AbstractFunction):
    """ A primitive function, i.e. one not implemented in Haskell but at the
    machine level. """
    _immutable_fields_ = ["name", "prim_function", "arity", "strictargs"]

    def __init__(self, function, arity, strictargs):
        AbstractFunction.__init__(self)
        self.prim_function = function
        self.name = function.func_name
        self.arity = arity
        self.strictargs = strictargs

    @jit.unroll_safe
    def apply(self, application, todo):
        args = [None] * self.arity
        assert application.numargs() == self.arity
        for i in range(self.arity):
            arg = application.getarg(i)
            strictarg = self.strictargs[i]
            if strictarg:
                value = arg.getvalue()
                if value is None:
                    return application.eval_after(arg, i, todo)
                args[i] = value
            else:
                args[i] = arg
        return self.prim_function(args), todo

    def tostr(self):
        return self.name

class Var(HaskellObject):
    def __init__(self, name):
        self.name = name

    def match(self, obj, subst):
        raise NotImplementedError

    def substitute(self, subst):
        raise NotImplementedError

    def enumerate_head(self, subst):
        res = NumberedVar(len(subst), self.name)
        subst[self] = res
        return res

    def replace_vars(self, subst, occurrences):
        res = subst[self]
        occurrences[res.index] += 1
        return res

    def tostr(self):
        return self.name

class NumberedVar(HaskellObject):
    _immutable_fields_ = ["name", "index"]
    def __init__(self, index, name=""):
        self.index = index
        self.name = name

    def match(self, obj, subst):
        assert subst[self.index] is None
        subst[self.index] = obj
        return DEFINITE_MATCH

    def substitute(self, subst):
        arg = subst[self.index]
        value = arg.getvalue()
        if value is not None:
            arg = value
        return arg

    def tostr(self):
        return self.name

class Application(HaskellObject):
    """ A function application. This is an abstract base class, there are
    subclasses generated below for various numbers of arguments."""
    _immutable_fields_ = ["function"]

    def __init__(self, function):
        self.function = function

    def step(self, todo):
        value = self.function.getvalue()
        if value:
            value = jit.hint(value, promote=True)
            print repr(value)
            #assert isinstance(value, AbstractFunction)
            return value.apply(self, todo)

        # higher order
        return self.eval_after(self.function, -1, todo)

    def eval_after(self, arg, i, todo):
        todo = CopyStackElement(self, i, todo)
        return arg, todo

    def replace(self, index, value):
        if index == -1:
            return make_application(value, self.getargs())
        args = self.getargs()
        index = jit.hint(index, promote=True)
        args[index] = value
        return make_application(self.function, args)

    @jit.unroll_safe
    def substitute(self, subst):
        args = [None] * self.numargs()
        for i in range(self.numargs()):
            args[i] = self.getarg(i).substitute(subst)
        res = make_application(self.function.substitute(subst), args)
        return res

    def replace_vars(self, subst, occurrences):
        args = [self.getarg(i).replace_vars(subst, occurrences)
                    for i in range(self.numargs())]
        return make_application(self.function.replace_vars(subst, occurrences),
                                args)

    def tostr(self):
        return "(%s %s)" % (self.function.tostr(), " ".join([arg.tostr() for arg in self.getargs()]))

    @jit.unroll_safe
    def getargs(self):
        args = [None] * self.numargs()
        for i in range(self.numargs()):
            args[i] = self.getarg(i)
        return args


specialized_app_classes = [make_arg_subclass(i, Application) for i in range(11)]

class ApplicationN(Application):
    _immutable_fields_ = ["args[*]"]

    def __init__(self, function, args):
        Application.__init__(self, function)
        self.args = args

    def numargs(self):
        return len(self.args)

    def getarg(self, i):
        return self.args[i]

def make_application(function, args):
    if len(args) < len(specialized_app_classes):
        cls = specialized_app_classes[len(args)]
    else:
        cls = ApplicationN
    return cls(function, args)

class Thunk(HaskellObject):
    """ An unevaluated function application. """
    def __init__(self, application):
        assert isinstance(application, Application)
        self.application = application

    def step(self, todo):
        app = self.application
        if isinstance(app, Value):
            return app, todo
        return self.application, UpdateStackElement(self, todo)

    def getvalue(self):
        app = self.application
        if isinstance(app, Value):
            return app
        return None

    def substitute(self, subst):
        raise NotImplementedError

    def tostr(self):
        if isinstance(self.application, Value):
            return self.application.tostr()
        return "?%s" % (self.application, )

# _________________________________________________________________
# head normal form stackless

def get_printable_location(function):
    if function is None:
        return "None"
    return function.tostr()

jitdriver = jit.JitDriver(
        greens=["function"],
        reds=["todo", "expr"],
        get_printable_location=get_printable_location,
        )



class StackElement(object):
    """ Base class of the stack elements of the evaluation stack. The content
    of the stack roughly corresponds to the Launchbury semantics. """
    def __init__(self, next):
        self.next = next

    def step(self, value):
        raise NotImplementedError("abstract base class")

class CopyStackElement(StackElement):
    """ Need to copy the top of the stack. """
    def __init__(self, application, index, next):
        StackElement.__init__(self, next)
        self.application = application
        self.index = index

    def step(self, value):
        return self.application.replace(self.index, value), self.next

    def __str__(self):
        return "copy(%s, %s, %s)" % (self.application, self.index, self.next)

class UpdateStackElement(StackElement):
    """ Need to update the thunk stored in this after its content has been
    evaluated. """
    def __init__(self, thunk, next):
        StackElement.__init__(self, next)
        self.thunk = thunk

    def step(self, value):
        assert self.thunk.getvalue() is None
        self.thunk.application = value
        return value, self.next

    def __str__(self):
        return "upd(%s, %s)" % (self.thunk, self.next)

def evaluate_hnf(obj):
    assert isinstance(obj, Application)
    return main_loop(obj)



def main_loop(expr):
    function = None
    todo = None
    i=0
    while True:
        #print "meh"
        jitdriver.jit_merge_point(function=function, todo=todo, expr=expr)
        if isinstance(expr, Substitution):
            expr = expr.apply()
        if isinstance(expr, Value) and todo is None:
            break
        #print expr, todo
        #import pdb; pdb.set_trace()
        print repr(expr)
        expr, todo = expr.step(todo)
        i=i+1
        print i
        function = None
        if isinstance(expr, Substitution):
            recursive = expr.recursive
            #print recursive
            function = expr.rhs
            #print repr(function)
            #print function.name
            if recursive:
                #print "can enter jit", function, expr
                jitdriver.can_enter_jit(function=function, todo=todo, expr=expr)
    return expr



# _________________________________________________________________
# primitives


def cons(a, b):
    return constr("Cons", a, b)


w_true = constr("True")
w_false = constr("False")
w_nil = constr("Nil")

def expose_primitive(arity, strictargs=None):
    if strictargs is None:
        strictargs = [True] * arity
    def f(primitive):
        return PrimFunction(primitive, arity, strictargs)
    return f

@expose_primitive(2)
def prim_add(args):
    a0, a1 = args
    assert isinstance(a0, Integer)
    assert isinstance(a1, Integer)
    return Integer(a0.value + a1.value)

@expose_primitive(2)
def prim_sub(args):
    a0, a1 = args
    assert isinstance(a0, Integer)
    assert isinstance(a1, Integer)
    return Integer(a0.value - a1.value)

@expose_primitive(2)
def prim_gt(args):
    a0, a1 = args
    assert isinstance(a0, Integer)
    assert isinstance(a1, Integer)
    if a0.value > a1.value:
        return w_true
    return w_false

@expose_primitive(2)
def prim_le(args):
    a0, a1 = args
    assert isinstance(a0, Integer)
    assert isinstance(a1, Integer)
    if a0.value <= a1.value:
        return w_true
    return w_false

@expose_primitive(2)
def prim_eq(args):
    a0, a1 = args
    assert isinstance(a0, Integer)
    assert isinstance(a1, Integer)
    if a0.value == a1.value:
        return w_true
    return w_false

@expose_primitive(2)
def prim_ne(args):
    a0, a1 = args
    assert isinstance(a0, Integer)
    assert isinstance(a1, Integer)
    if a0.value != a1.value:
        return w_true
    return w_false

@expose_primitive(2, strictargs=[True, False])
def prim_seq(args):
    assert args[0].getvalue() is not None
    return args[1]
