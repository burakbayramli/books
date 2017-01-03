#!/usr/bin/env python

# NOTE: As of 2007.09.05, there is an error in PrmDictBase
# triggered by this script

import os, re
from scitools.numpyutils import *
import scitools.misc
from scitools.PrmDictBase import PrmDictBase

class SomeSolver(PrmDictBase):
    def __init__(self, **kwargs):
        PrmDictBase.__init__(self)
        # register parameters in dictionaries:
        self.physical_prm = {'density': 1.0, 'Cp': 1.0,
                                   'k': 1.0, 'L': 1.0}
        self.numerical_prm =  {'n': 10, 'dt': 0.1, 'tstop': 3}

        # attach dictionaries to base class list (required):
        self._prm_list = [self.physical_prm, self.numerical_prm]

        # specify parameters to be type checked when set:
        # (this stopped working...)
        #self._type_check.update({'n': True, 'dt': (float,),
        #      'k': lambda k: isinstance(int,float) and k>0})

        # disallow arbitrary meta data
        self.user_prm = None # set to {} if meta data are allowed

        # initialize parameters according to keyword arguments:
        self.set(**kwargs)

    def _update(self):
        # dt depends on n, L, k; update dt in case the three
        # others parameters have been changed
        # (in general this method is used to check consistency
        # between parameters and perform updates if necessary)
        n = self.numerical_prm['n']
        L = self.physical_prm['L']
        k = self.physical_prm['k']

        self.u = zeros(n+1)
        h = L/float(n)
        dt_limit = h**2/(2*k)
        if self.numerical_prm['dt'] > dt_limit:
            self.numerical_prm['dt'] = dt_limit

    def compute1(self):
        # compute something
        return self.physical_prm['k']/self.physical_prm['Cp']
    
    def compute2(self):
        # turn numerical parameters into local variables:
        exec self.dicts2variables(self._prm_list)
        # or exec self.dicts2variables(self.numerical_prm)  # selected prms

        # now we have local variables n, dt, tstop, density, Cp, k, L
        # that we can compute with, say

        Q = k/Cp
        dt = 0.9*dt

        # if some of the local variables are changed, say dt, they must
        # be inserted back into the parameter dictionaries:
        self.variables2dicts(self.numerical_prm, dt=dt)

    def test_short_forms(self):
        """
        # this doesn't work with locals():
        self.dicts2namespace(locals(), self._prm_list)
        print 'local variables:', locals()
        try:
            print 'n =', n
        except NameError, msg:
            print msg
        self.namespace2dicts(locals(), self._prm_list) # clean up

        # exec with locals() from this function: (doesn't work)
        self.dicts2namespace2(locals(), self._prm_list)
        try:
            print 'after exec: n =', n
        except NameError, msg:
            print msg
        # no clean up

        # this works fine:
        exec '%s=%s' % ('n', repr(self.numerical_prm['n']))
        try:
            print 'after local exec: n =', n
        except NameError, msg:
            print msg

        # manipulate locals() here:
        locals()['Cp'] = 1.1
        try:
            print 'locals()[..] modification, Cp =', Cp
        except NameError, msg:
            print msg
        """
        # globals() work:
        self.dicts2namespace(globals(), self._prm_list)
        #kk = globals().keys(); kk.sort(lambda a,b: cmp(a.lower(),b.lower()))
        #print kk
        print 'dt =', dt
        self.namespace2dicts(globals(), self._prm_list) # clean up

        # can make attributes too:
        self.dicts2namespace(self.__dict__, self._prm_list)
        print 'self.n =', self.n
        print 'self.dt =', self.dt
        self.namespace2dicts(self.__dict__, self._prm_list) # clean up

        # exec in a special dictionary:
        mydict = {}
        self.dicts2namespace(mydict, self._prm_list)
        code = """\
print 'Cp=', Cp, 'n=', 'L=', L"""
        exec code in globals(), mydict
        # safe
        

def _test1():
    s = SomeSolver(k=2)
    s.set(Cp=0.1, n=100)
    s.set()  # usage message

    # copy to local variables:
    print 'original local variables:', locals().keys()
    exec s.dicts2variables(s.physical_prm)
    print 'new local variables:', locals().keys()
    Cp = 0.99
    L = 100
    s.variables2dicts(s.physical_prm, Cp=Cp, L=L)
    s.dump()
    s.usage()
    
    print 'is s extensible?', s.user_prm is dict
    try:
        s.set(q=0.1, m=100)
    except NameError, msg:
        print msg
    s.user_prm = {}
    print 'is s extensible?', s.user_prm is dict
    s.set(q=0.1, m=100)
    s.properties(globals())
    print 's.m:', s.m
    print 's.Cp:', s.Cp
    print 's.L:', s.L
    try:
        s.Cp = 0.1
    except AttributeError, msg:
        print msg
    s.dicts2namespace(locals(), s._prm_list)
    print 'local variables:', locals()
    # cannot access L, Cp, ... :-(
    s.test_short_forms()
    
if __name__ == '__main__':
    _test1()
                            
