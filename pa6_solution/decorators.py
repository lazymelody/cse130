from misc import Failure

class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__f=f
        level = ''
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0

class traced(object):
    """ traced is a class for a decorator that when called it prints 
    out an ASCII art tree of the recursive calls and their return values"""
    count = 0
    """ this is the ctor for the traced class, it initializes the values
    for the variables """
    def __init__(self,f):
        self.f = f
        self.__name__=f.__name__
        self.result = None
        self.count = 0
    """ this is the call func of the class, it is called when the the decorator
    is used, there is a try catch block to check for execptions and if one is 
    caugt the count is decremented. """
    def __call__(self, *args, **kwargs):
        
        try:
            level = '' 
            level = level + ('| ' * traced.count)
            level = level + ',- '     
            level = level + self.__name__ + '('      
            level = level +  ', '.join([repr(arg) for arg in args])
            level = level + ', '.join([key + '=' + repr(val) for key, val in kwargs.items()])
            level = level + ')'
            print level
            traced.count = traced.count + 1
            total = self.f(*args, **kwargs)
        
        except Exception as e: 
            traced.count = traced.count - 1
            raise e  

        if total:
           self.total = total
        traced.count  = traced.count - 1
        level = ''
        level = level + ('| ' * traced.count)
        level = level + '`- ' + repr(total)
        print level
        return total

class memoized(object):
    """ memoized is a class for a decorator that when called checks to see if 
    the given function has already been called with the arguement and if so
    the precomputed result is returned """
    val = { }
    """ this is the ctor for the memoized class that initializes the default
    values """
    def __init__(self, f):
        self.__name__ = f.__name__
        self.f = f

    """ this is the call func for the memoized class, it is called every time 
    the decorator is used, uses a try catch block to catch exceptions and also 
    checks to see if the value is an instance of the Exception class and if
    it is raises the value that causes the exception """
    def __call__(self, *args):
        key = (self.__name__, str(args))
        try:
            if not self.val.has_key(key):        
                self.val[key] = self.f(*args)
        except Exception as e:
            self.val[key] = e
        if isinstance(self.val[key] , Exception):
            raise self.val[key]          
        return self.val[key]   

def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print "RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a]))
        rv=f(*a)
        print "RETURNED %s" % repr(rv)

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)


