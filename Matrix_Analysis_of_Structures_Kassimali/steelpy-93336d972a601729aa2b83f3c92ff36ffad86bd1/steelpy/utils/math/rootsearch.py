# Copyright (c) 2015-2016 steelpy

# Python stdlib imports
import math

# package imports


#
#-------------------------------------------------
#                Supporting Section
#-------------------------------------------------
#
# module error
# 
def err(string):
    """ err(string).
    Prints string and terminates program.
    """
    print (string)
    raw_input("Press return to exit")
    exit()
#
#
# module rootsearch
#
def rootsearch(f, a, b, dx):
    """ x1,x2 = rootsearch(f,a,b,dx).
    Searches the interval (a,b) in increments dx for
    the bounds (x1,x2) of the smallest root of f(x).
    Returns x1 = x2 = None if no roots were detected.
    """
    x1 = a
    f1 = f(a)
    x2 = a + dx
    f2 = f(x2)
    #
    while f1*f2 > 0.0:
        if x1 >= b: 
            return None, None
        x1 = x2
        f1 = f2
        #x2 = x1 + dx
        x2 += dx
        f2 = f(x2)
    return x1, x2
#
#
#
#
# module bisect
#
def bisect(f, x1, x2, switch=0, epsilon=1.0e-9):
    """ root = bisect(f,x1,x2,switch=0,tol=1.0e-9).
    Finds a root of f(x) = 0 by bisection.
    The root must be bracketed in (x1,x2).
    Setting switch = 1 returns root = None if
    f(x) increases as a result of a bisection.
    """
    f1 = f(x1)
    try:
        1/f1
    except ZeroDivisionError:
        return x1
    #
    f2 = f(x2)
    try:
        1/f2
    except ZeroDivisionError:
        return x2
    
    if f1*f2 > 0.0: 
        err("Root is not bracketed")
    #
    n = int(math.ceil(math.log(abs(x2 - x1)/epsilon)/math.log(2.0)))
    #
    for i in range(n):
        x3 = 0.5*(x1 + x2)
        f3 = f(x3)
        #
        if switch == 1 and abs(f3) > abs(f1) and abs(f3) > abs(f2):
            return None
        try:
            1/f3
        except ZeroDivisionError:
            return x3
        
        if f2*f3 < 0.0:
            x1 = x3
            f1 = f3
        else:
            x2 =x3
            f2 = f3
    return (x1 + x2)/2.0
#
#
#
def PipeTypeFinder(PipeType):
    #
    # Riser
    if PipeType == 'RISER':
        PipeTypeFinal = 'RISER'
    #
    elif PipeType == 'LANDFALL':
        PipeTypeFinal = 'RISER'
    #
    elif PipeType == 'RISER/LANDFALL':
        PipeTypeFinal = 'RISER'
    #
    # Seabed
    elif PipeType == 'SEABED':
        PipeTypeFinal = 'SEABED'
    #
    elif PipeType == 'TIE-IN':
        PipeTypeFinal = 'SEABED'
    #
    elif PipeType == 'SEABED INCLUDING TIE-IN':
        PipeTypeFinal = 'SEABED'
    #
    # Type no found then close & exit program
    else:
        print ('Pipe type Not Recognised')
        exit()
    #
    #
    return PipeTypeFinal
    #
    #
#
#
def DesignCondFinder(DesignCond):
    #
    # Riser
    if DesignCond == 'HYDROTEST':
        DesignCondFinal = 'HYDROTEST'
    #
    elif DesignCond == 'CONSTRUCTION':
        DesignCondFinal = 'HYDROTEST'
    #
    else:
        DesignCondFinal = DesignCond 
    #
    #
    return DesignCondFinal
    #
    #
#
#
#
def GoalSeeker(f, bracket, search="FULL", step=0.01):
    """
    """
    a,b,dx = (0.0, bracket, step)
    
    if search.upper() != "FULL":
        x1,x2 = rootsearch(f, a, b, dx)
        root = bisect(f,x1,x2,1)
    else:
        Roots = []
        while 1:
            # search for the bounds
            x1,x2 = rootsearch(f, a, b, dx)
            # check if root exist
            if x1 != None:
                a = x2
                root = bisect(f,x1,x2,1)
                # if root found, report it
                if root != None and root > 0.0:
                    #print (root)
                    # if root is real positive
                    #if root > 0.0:
                    Roots.append(root)
            # 
            # if no more roots found, stop
            else:
                break
        root = min(Roots)
    #print ("Print min root",root)
    return root
#
#
#