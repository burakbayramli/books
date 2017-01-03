# export PYTHONPATH=/home/andy/projects/hmmds3/code/
# export PYTHONPATH=/home/andy/projects/hmmds3/code/hmm/:$PYTHONPATH
# Copyright (c) 2013 Andrew M. Fraser
import numpy as np
from hmm.Scalar import initialize, Prob, Discrete_Observations, Class_y
from hmm.Scalar import make_prob
from hmm.base import HMM
from numpy.testing import assert_, assert_allclose, run_module_suite
from scipy.linalg import circulant
import C

c2s = {
    0:[0,1],
    1:[2,3],
    2:[4,5],
    }
P_S0 = np.ones(6)/6.0
P_S0_ergodic = np.ones(6)/6.0
P_SS = circulant([0,  0, 0, 0, .5, .5])
P_YS = circulant([.4, 0, 0, 0, .3, .3])
class TestHMM:
    def __init__(self):
        self.mod = HMM(
            P_S0.copy(), P_S0_ergodic.copy(), P_YS.copy(), P_SS.copy())
        self.Cmod = C.HMM(
            P_S0.copy(), P_S0_ergodic.copy(), P_YS.copy(), P_SS.copy())
        self.Smod = C.HMM_SPARSE(
            P_S0.copy(), P_S0_ergodic.copy(), P_YS.copy(), P_SS.copy())
        self.mods = (self.mod, self.Cmod, self.Smod)
        self.S,Y = self.mod.simulate(1000)
        Y = (np.array(Y[0], np.int32),)
        self.Y = Y
        return
    def decode(self, mod):
        E = np.where(mod.decode(self.Y) != self.S)[0]
        assert_(len(E) < 300)
        return
    def test_decode(self):
        for mod in self.mods:
            self.decode(mod)
        return
    def train(self, mod):
        L = mod.train(self.Y,n_iter=10, display=False)
        for i in range(1,len(L)):
            assert_(L[i-1] < L[i])
        assert_allclose(mod.y_mod.P_YS.values(), P_YS, atol=0.08)
        assert_allclose(mod.P_SS.values(), P_SS, atol=0.2)
        return L
    def test_train(self):
        Ls= []
        for mod in self.mods:
            Ls.append(self.train(mod))
        return
    def multi_train(self, mod):
        ys = []
        for i in [1,2,0,4,3]:
            ys.append([x[200*i:200*(i+1)] for x in self.Y])
        L = mod.multi_train(ys, n_iter=10, display=False)
        for i in range(1,len(L)):
            assert_(L[i-1] < L[i])
        assert_allclose(mod.y_mod.P_YS.values(), P_YS, atol=0.08)
        assert_allclose(mod.P_SS.values(),       P_SS, atol=0.2)
    def test_multi_train(self):
        for mod in self.mods:
            self.multi_train(mod)
class TestHMM_classy:
    def __init__(self):
        pars = (Discrete_Observations, P_YS, c2s)
        self.mod = HMM(P_S0, P_S0, pars, P_SS, Class_y, make_prob)
        self.S,CY = self.mod.simulate(1000)
        self.CY = [np.array(CY[0], np.int32), np.array(CY[1], np.int32)]
        p_s = 0.7*P_SS + 0.3/6
        p_y = 0.7*P_YS + 0.3/6
        pars = (Discrete_Observations, p_y, c2s)
        self.mod_t = HMM(P_S0, P_S0, pars, p_s, Class_y, make_prob)
        self.L = self.mod_t.train(CY, n_iter=20, display=False)
    def test_train(self):
        for i in range(1,len(self.L)):
            assert_(self.L[i-1] < self.L[i])
        assert_allclose(self.mod_t.y_mod.y_mod.P_YS, P_YS, atol=0.08)
        assert_allclose(self.mod_t.P_SS, P_SS, atol=0.16)
    def test_decode(self):
        D = self.mod.class_decode((self.CY[1],))
        E = np.where(D != self.CY[0])[0]
        assert_(len(E) < 150)

if __name__ == "__main__":
    run_module_suite()

#--------------------------------
# Local Variables:
# mode: python
# End:
