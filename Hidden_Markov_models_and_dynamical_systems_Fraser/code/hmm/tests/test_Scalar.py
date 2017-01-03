# export PYTHONPATH=/home/andy/projects/hmmds3/code/
# export PYTHONPATH=/home/andy/projects/hmmds3/code/hmm/:$PYTHONPATH
# Copyright (c) 2013 Andrew M. Fraser
import numpy as np
import Scalar
import C as Sparse
from numpy.testing import assert_, assert_allclose, assert_almost_equal
from numpy.testing import run_module_suite, assert_equal
from scipy.linalg import circulant
import C

A = [
    [0, 2, 2.0],
    [2, 2, 4.0],
    [6, 2, 2.0]]
B = [
    [0, 1],
    [1, 1],
    [1, 3.0]]
C = [
    [0, 0, 2.0],
    [0, 0, 1.0],
    [6, 0, 0.0]]
class TestScalar:
    def __init__(self):
        self.A = Scalar.make_prob(A)
        self.B = Scalar.make_prob(B)
        self.C = Scalar.make_prob(C)
        self.A_s = Sparse.make_prob(self.A)
        self.B_s = Sparse.make_prob(self.B)
        self.C_s = Sparse.make_prob(self.C)
        self.Ms = (self.A, self.B, self.C, self.A_s, self.B_s, self.C_s)
        for M in self.Ms:
            M.normalize()
        return
    def test_normalize(self):
        for M in self.Ms:
            m,n = M.shape
            for i in range(m):
                s = 0
                for j in range(n):
                    s += M.values()[i,j]
                assert_almost_equal(1, s)
        return
    def assign(self, M):
        a = M.values().sum()
        M.assign_col(1, [1, 1, 1])
        assert_almost_equal(M.values().sum(), a+3)
        return
    def test_assign(self):
        for M in (self.C, self.C_s):
            self.assign(M)
        return
    def likelihoods(self, M):
        assert_allclose(M.likelihoods([0,1,2])[2], [1,1,0])
        return
    def test_likelihoods(self):
        for M in (self.C, self.C_s):
            self.likelihoods(M)
        return
    def cost(self, M):
        assert_almost_equal(M.cost(self.B.T[0], self.B.T[1]),
                            [[ 0, 0, 0], [0, 0, .375], [0.25, 0, 0]])
        return
    def test_cost(self):
        for M in (self.C, self.C_s):
            self.cost(M)
        return
    def inplace_elementwise_multiply(self, M):
        M.inplace_elementwise_multiply(self.A)
        assert_almost_equal(M.values(), [[ 0, 0, .5], [0, 0, 0.5], [0.6, 0, 0]])
        return
    def test_inplace_elementwise_multiply(self):
        for M in (self.C, self.C_s):
            self.inplace_elementwise_multiply(M)
        return
    def step_forward(self, M):
        B = self.B.T[1].copy()
        M.step_forward(B)
        assert_almost_equal(B, [ 0.575,  0.775,  0.9  ])
    def test_step_forward(self):
        for M in (self.A, self.A_s):
            self.step_forward(M)
    def step_back(self, M):
        B = self.B.T[1].copy()
        M.step_back(B)
        assert_almost_equal(B, [ 0.625,  0.75,  0.85  ])
    def test_step_back(self):
        for M in (self.A, self.A_s):
            self.step_back(M)
    def values(self, M):
        assert_almost_equal(M.values(), [[0,0,1],[0,0,1],[1,0,0]])
    def test_values(self):
        for M in (self.C, self.C_s):
            self.values(M)
class Test_Discrete_Observations:
    def __init__(self):
        P_YS = Scalar.make_prob(B)
        P_YS.normalize()
        self.y_mod = Scalar.Discrete_Observations(P_YS)
        P_YS_s = Sparse.make_prob(P_YS)
        self.y_mod_s = Sparse.Discrete_Observations(P_YS)
        N = 20
        Y = np.empty(N, dtype=np.int32)
        for i in range(N):
            Y[i] = (i + i%2 + i%3 + i%5)%2
        self.Y = [Y]
        self.w = np.array(20*[0,0,1.0]).reshape((N,3))
        self.w[0,:] = [1,0,0]
        self.w[3,:] = [0,1,0]
        self.Ys = [[Y[5:]],[Y[3:7]],[Y[:4]]]
    def calc(self, y_mod):
        PY = y_mod.calc(self.Y)[2:4]
        assert_almost_equal(PY, [[ 0, 0.5, 0.25],[ 1, 0.5, 0.75]])
    def test_calc(self):
        for y_mod in (self.y_mod, self.y_mod_s):
            self.calc(y_mod)
    def join(self, y_mod):
        n_seg, t_seg, y_all  = y_mod.join(self.Ys)
        assert_equal(n_seg, 3)
        assert_equal(t_seg, [0, 15, 19, 23])
    def test_join(self):
        for y_mod in (self.y_mod, self.y_mod_s):
            self.join(y_mod)
    def reestimate(self, y_mod):
        y_mod.reestimate(self.w, self.Y)
        assert_almost_equal([[1, 0],[0, 1],[5/9, 4/9]], y_mod.P_YS.values())
    def test_reestimate(self):
        for y_mod in (self.y_mod, self.y_mod_s):
            self.reestimate(y_mod)

if __name__ == "__main__":
    run_module_suite()

#--------------------------------
# Local Variables:
# mode: python
# End:
