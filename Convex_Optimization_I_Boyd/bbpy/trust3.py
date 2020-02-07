from __future__ import division, print_function, absolute_import
#################################################################3
# all this came from ip-nonlinear-solver
# for only non-constrained problems.
from math import copysign
import numpy as np
from numpy.linalg import norm
from scipy.sparse import eye, bmat, issparse, csc_matrix, csr_matrix, coo_matrix, find
from scipy.optimize._group_columns import group_dense, group_sparse
import numpy as np
import scipy.sparse as spc
import numpy as np
from warnings import warn
from copy import deepcopy
import scipy.sparse as spc
import time
from scipy.optimize import OptimizeResult
sksparse_available = False
import scipy.sparse.linalg

EPS = np.finfo(np.float64).eps

def default_scaling(x):
    n, = np.shape(x)
    return spc.eye(n)
    
class Rosenbrock:
    def __init__(self, n=2, random_state=0):
        rng = np.random.RandomState(random_state)
        self.x0 = rng.uniform(-1, 1, n)
        self.x_opt = np.ones(n)

    def fun(self, x):
        x = np.asarray(x)
        r = np.sum(100.0 * (x[1:] - x[:-1]**2.0)**2.0 + (1 - x[:-1])**2.0,
                   axis=0)
        return r

    def grad(self, x):
        x = np.asarray(x)
        xm = x[1:-1]
        xm_m1 = x[:-2]
        xm_p1 = x[2:]
        der = np.zeros_like(x)
        der[1:-1] = (200 * (xm - xm_m1**2) -
                     400 * (xm_p1 - xm**2) * xm - 2 * (1 - xm))
        der[0] = -400 * x[0] * (x[1] - x[0]**2) - 2 * (1 - x[0])
        der[-1] = 200 * (x[-1] - x[-2]**2)
        return der

    def hess(self, x):
        x = np.atleast_1d(x)
        H = np.diag(-400 * x[:-1], 1) - np.diag(400 * x[:-1], -1)
        diagonal = np.zeros(len(x), dtype=x.dtype)
        diagonal[0] = 1200 * x[0]**2 - 400 * x[1] + 2
        diagonal[-1] = 200
        diagonal[1:-1] = 202 + 1200 * x[1:-1]**2 - 400 * x[2:]
        H = H + np.diag(diagonal)
        return H

    @property
    def constr(self):
        lb = np.array([0, 0])
        ub = np.array([0.5, 0.5])
        kind = ("interval", lb, ub)
        box = BoxConstraint(kind)
        return box


TERMINATION_MESSAGES = {
    0: "The maximum number of function evaluations is exceeded.",
    1: "`gtol` termination condition is satisfied.",
    2: "`xtol` termination condition is satisfied.",
    3: "`callback` function requested termination"
}

def _check_kind(kind, m):
    if not isinstance(kind, (tuple, list, str)):
        raise ValueError("The parameter `kind` should be a tuple, "
                         " a list, or a string.")
    if isinstance(kind, str):
        kind = (kind,)
    if len(kind) == 0:
        raise ValueError("The parameter `kind` should not be empty.")

    n_args = len(kind)
    keyword = kind[0]
    if keyword not in ("greater", "less", "equals", "interval"):
        raise ValueError("Keyword `%s` not available." % keyword)
    if n_args in (1, 2) and keyword not in ("greater", "less", "equals") \
       or n_args == 3 and keyword not in ("interval"):
        raise ValueError("Invalid `kind` format.")
    if n_args == 1:
        kind = (keyword, 0)

    if keyword in ("greater", "less", "equals"):
        c = np.asarray(kind[1], dtype=float)
        if np.size(c) not in (1, m):
            if keyword == "greater":
                raise ValueError("`lb` has the wrong dimension.")
            if keyword == "less":
                raise ValueError("`ub` has the wrong dimension.")
            if keyword == "equals":
                raise ValueError("`c` has the wrong dimension.")
        c = np.resize(c, m)
        return (keyword, c)
    elif keyword == "interval":
        lb = np.asarray(kind[1], dtype=float)
        if np.size(lb) not in (1, m):
            raise ValueError("`lb` has the wrong dimension.")
        lb = np.resize(lb, m)
        ub = np.asarray(kind[2], dtype=float)
        if np.size(ub) not in (1, m):
            raise ValueError("`ub` has the wrong dimension.")
        ub = np.resize(ub, m)
        if (lb > ub).any():
            raise ValueError("lb[i] > ub[i].")
        return (keyword, lb, ub)


def _check_enforce_feasibility(enforce_feasibility, m):
    if isinstance(enforce_feasibility, bool):
        enforce_feasibility = np.full(m,
                                      enforce_feasibility,
                                      dtype=bool)
    else:
        enforce_feasibility = np.array(enforce_feasibility,
                                       dtype=bool)

        if enforce_feasibility.size != m:
            raise ValueError("The parameter 'enforce_feasibility' "
                             "has the wrong number of elements.")
    return enforce_feasibility


def _is_feasible(kind, enforce_feasibility, f0):
    keyword = kind[0]
    if keyword == "equals":
        lb = np.asarray(kind[1], dtype=float)
        ub = np.asarray(kind[1], dtype=float)
    elif keyword == "greater":
        lb = np.asarray(kind[1], dtype=float)
        ub = np.full_like(lb, np.inf, dtype=float)
    elif keyword == "less":
        ub = np.asarray(kind[1], dtype=float)
        lb = np.full_like(ub, -np.inf, dtype=float)
    elif keyword == "interval":
        lb = np.asarray(kind[1], dtype=float)
        ub = np.asarray(kind[2], dtype=float)
    else:
        raise RuntimeError("Never be here.")

    return ((lb[enforce_feasibility] <= f0[enforce_feasibility]).all()
            and (f0[enforce_feasibility] <= ub[enforce_feasibility]).all())

def _parse_constraint(kind):
    """Read constraint type and return list of indices.

    Parameters
    ----------
    kind : tuple
        Specifies the type of contraint. Options for this
        parameters are:

            - ("interval", lb, ub): for a constraint of the type:
                lb[i] <= f[i] <= ub[i]
            - ("greater", lb): for a constraint of the type:
                f[i] >= lb[i]
            - ("less", ub): for a constraint of the type:
                f[i] <= ub[i]
            - ("equals", c): for a constraint of the type:
                f[i] == c[i]

        where ``lb``,  ``ub`` and ``c`` are (m,) ndarrays.
    Returns
    -------
    eq : array_like
        A vector indicating equality constraints.
            len(eq) = number of equality constraints
    ineq : array_like
        A vector indicating inequality constraints.
            len(ineq) = number of inequality constraints
    val_eq : array_like
        Equality constraint right hand side:
            f[eq[i]] = val_eq[i]
    val_ineq : array_like
        Inequality constraint right hand side:
            sign[i]*(f[ineq[i]] - val_ineq[i]) <= 0
    sign : array_like
        Sign of inequality constraints.
    """

    if kind[0] == "equals":
        # Read values from input structure
        c = np.asarray(kind[1], dtype=float)
        # Set returns
        eq = np.arange(len(c), dtype=int)
        ineq = np.empty(0, dtype=int)
        val_eq = np.asarray(c)
        val_ineq = np.empty(0)
        sign = np.empty(0)
        fun_len = len(c)
    elif kind[0] in ("greater", "less", "interval"):
        # Constraint type
        if kind[0] == "greater":
            lb = np.asarray(kind[1], dtype=float)
            ub = np.full_like(lb, np.inf, dtype=float)
        elif kind[0] == "less":
            ub = np.asarray(kind[1], dtype=float)
            lb = np.full_like(ub, -np.inf, dtype=float)
        elif kind[0] == "interval":
            lb = np.asarray(kind[1], dtype=float)
            ub = np.asarray(kind[2], dtype=float)
        # Set auxiliar values
        arange = np.arange(len(lb), dtype=int)
        ones = np.ones(len(lb))
        lb_isinf = np.isinf(lb)
        ub_isinf = np.isinf(ub)
        eq_list = (lb == ub) & ~lb_isinf & ~ub_isinf
        # Set returns
        eq = arange[eq_list]
        val_eq = lb[eq_list]
        ineq = np.hstack((arange[~eq_list & ~lb_isinf],
                          arange[~eq_list & ~ub_isinf]))
        val_ineq = np.hstack((lb[~eq_list & ~lb_isinf],
                              ub[~eq_list & ~ub_isinf]))
        sign = np.hstack((-ones[~eq_list & ~lb_isinf],
                          ones[~eq_list & ~ub_isinf]))
        fun_len = len(lb)
    else:
        raise RuntimeError("Never be here.")

    return eq, ineq, val_eq, val_ineq, sign, fun_len

def _convert_constr(c, n_vars, n_eq, n_ineq,
                    eq, ineq, val_eq, val_ineq,
                    sign):
    # Empty constraint
    empty = np.empty((0,))
    # Return equality and inequalit constraints
    c_eq = c[eq] - val_eq if n_eq > 0 else empty
    c_ineq = sign*(c[ineq] - val_ineq) if n_ineq > 0 else empty
    return c_ineq, c_eq

def _convert_dense_jac(J, n_vars, n_eq, n_ineq,
                       eq, ineq, val_eq, val_ineq,
                       sign):
    # Empty jacobian
    empty = np.empty((0, n_vars))
    # Compute equality and inequality Jacobian matrices
    J_eq = J[eq, :] if n_eq > 0 else empty
    if n_ineq > 0:
        J_ineq = np.multiply(J[ineq, :], sign[:, np.newaxis])
    else:
        J_ineq = empty
    # Return Jacobian matrices
    return J_ineq, J_eq


def _box_to_canonical(box):
    return _linear_to_canonical(box.to_linear())

def _convert_sparse_jac(J, n_vars, n_eq, n_ineq,
                        eq, ineq, val_eq, val_ineq,
                        sign):
    # Empty jacobian
    empty = spc.csr_matrix(np.empty((0, n_vars)))
    # Compute equality and inequality Jacobian matrices
    J_eq = J[eq, :] if n_eq > 0 else empty
    if n_ineq > 0:
        D = spc.lil_matrix((n_ineq, n_ineq))
        D.setdiag(sign)
        J_ineq = D*J[ineq, :]
    else:
        J_ineq = empty
    # Return Jacobian matrices
    return J_ineq, J_eq



class LinearConstraint:
    """Linear constraint.

    Parameters
    ----------
    A : {ndarray, sparse matrix}, shape (m, n)
        Matrix for the linear constraint.
    kind : {str, tuple}
        Specifies the type of contraint. Options for this
        parameter are:

            - ('interval', lb, ub) for a constraint of the type:
                lb <= A x <= ub
            - ('greater', lb) for a constraint of the type:
                A x >= lb
            - ('less', ub) for a constraint of the type:
                A x <= ub
            - ('equals', c) for a constraint of the type:
                A x == c
            - ('greater',) for a constraint of the type:
                A x >= 0
            - ('less',) for a constraint of the type:
                A x <= 0
            - ('equals',) for a constraint of the type:
                A x == 0

        where ``lb``,  ``ub`` and ``c`` are (m,) ndarrays or
        scalar values. In the latter case, the same value
        will be repeated for all the constraints.
    enforce_feasibility : {list of boolean, boolean}, optional
        Specify if the constraint must be feasible along the iterations.
        If ``True``  all the iterates generated by the optimization
        algorithm need to be feasible in respect to a constraint. If ``False``
        this is not needed. A list can be passed to specify element-wise
        each constraints needs to stay feasible along the iterations and
        each does not. Alternatively, a single boolean can be used to
        specify the feasibility required of all constraints. By default it
        is False.
    """
    def __init__(self, A, kind, enforce_feasibility=False):
        self.A = A
        self.kind = kind
        self.enforce_feasibility = enforce_feasibility
        self.isinitialized = False

    def evaluate_and_initialize(self, x0, sparse_jacobian=None):
        if sparse_jacobian or (sparse_jacobian is None
                               and spc.issparse(self.A)):
            self.A = spc.csr_matrix(self.A)
            self.sparse_jacobian = True
        else:
            if spc.issparse(self.A):
                self.A = self.A.toarray()
            else:
                self.A = np.atleast_2d(self.A)
            self.sparse_jacobian = False

        x0 = np.atleast_1d(x0).astype(float)
        f0 = self.A.dot(x0)
        J0 = self.A

        self.x0 = x0
        self.f0 = f0
        self.J0 = J0
        self.n = x0.size
        self.m = f0.size
        self.kind = _check_kind(self.kind, self.m)
        self.enforce_feasibility \
            = _check_enforce_feasibility(self.enforce_feasibility, self.m)
        if not _is_feasible(self.kind, self.enforce_feasibility, f0):
            raise ValueError("Unfeasible initial point. "
                             "Either set ``enforce_feasibility=False`` or "
                             "choose a new feasible initial point ``x0``.")

        self.isinitialized = True
        return x0

    def to_nonlinear(self):
        if not self.isinitialized:
            raise RuntimeError("Trying to convert uninitialized constraint.")

        def fun(x):
            return self.A.dot(x)

        def jac(x):
            return self.A

        # Build Constraints
        nonlinear = NonlinearConstraint(fun, self.kind, jac, None,
                                        self.enforce_feasibility)
        nonlinear.isinitialized = True
        nonlinear.m = self.m
        nonlinear.n = self.n
        nonlinear.sparse_jacobian = self.sparse_jacobian
        nonlinear.fun = fun
        nonlinear.jac = jac
        nonlinear.hess = None
        nonlinear.x0 = self.x0
        nonlinear.f0 = self.f0
        nonlinear.J0 = self.J0
        return nonlinear

class NonlinearConstraint:
    """Nonlinear constraint

    Parameters
    ----------
    fun : callable
        The function defining the constraint.

            fun(x) -> array_like, shape (m,)

        where x is a (n,) ndarray and ``m``
        is the number of constraints.
    kind : {str, tuple}
        Specifies the type of contraint. Options for this
        parameter are:

            - ('interval', lb, ub) for a constraint of the type:
                lb <= fun(x) <= ub
            - ('greater', lb) for a constraint of the type:
                fun(x) >= lb
            - ('less', ub) for a constraint of the type:
                fun(x) <= ub
            - ('equals', c) for a constraint of the type:
                fun(x) == c
            - ('greater',) for a constraint of the type:
                fun(x) >= 0
            - ('less',) for a constraint of the type:
                fun(x) <= 0
            - ('equals',) for a constraint of the type:
                fun(x) == 0

        where ``lb``,  ``ub`` and ``c`` are (m,) ndarrays or
        scalar values. In the latter case, the same value
        will be repeated for all the constraints.
    jac : callable
        Jacobian Matrix:

            jac(x) -> {ndarray, sparse matrix}, shape (m, n)

        where x is a (n,) ndarray.
    hess : {callable, '2-point', '3-point', 'cs', None}
        Method for computing the Hessian matrix. The keywords
        select a finite difference scheme for numerical
        estimation. The scheme '3-point' is more accurate, but requires
        twice as much operations compared to '2-point' (default). The
        scheme 'cs' uses complex steps, and while potentially the most
        accurate, it is applicable only when `fun` correctly handles
        complex inputs and can be analytically continued to the complex
        plane. If it is a callable, it should return the 
        Hessian matrix of `dot(fun, v)`:

            hess(x, v) -> {LinearOperator, sparse matrix, ndarray}, shape (n, n)

        where x is a (n,) ndarray and v is a (m,) ndarray. When ``hess``
        is None it considers the hessian is an matrix filled with zeros.
    enforce_feasibility : {list of boolean, boolean}, optional
        Specify if the constraint must be feasible along the iterations.
        If ``True``  all the iterates generated by the optimization
        algorithm need to be feasible in respect to a constraint. If ``False``
        this is not needed. A list can be passed to specify element-wise
        each constraints needs to stay feasible along the iterations and
        each does not. Alternatively, a single boolean can be used to
        specify the feasibility required of all constraints. By default it
        is False.
    """
    def __init__(self, fun, kind, jac, hess='2-point', enforce_feasibility=False):
        self._fun = fun
        self.kind = kind
        self._jac = jac
        self._hess = hess
        self.enforce_feasibility = enforce_feasibility
        self.isinitialized = False

    def evaluate_and_initialize(self, x0, sparse_jacobian=None):
        x0 = np.atleast_1d(x0).astype(float)
        f0 = np.atleast_1d(self._fun(x0))
        v0 = np.zeros_like(f0)
        J0 = self._jac(x0)

        def fun_wrapped(x):
            return np.atleast_1d(self._fun(x))

        if sparse_jacobian or (sparse_jacobian is None and spc.issparse(J0)):
            def jac_wrapped(x):
                return spc.csr_matrix(self._jac(x))
            self.sparse_jacobian = True

            self.J0 = spc.csr_matrix(J0)

        else:
            def jac_wrapped(x):
                J = self._jac(x)
                if spc.issparse(J):
                    return J.toarray()
                else:
                    return np.atleast_2d(J)
            self.sparse_jacobian = False

            if spc.issparse(J0):
                self.J0 = J0.toarray()
            else:
                self.J0 = np.atleast_2d(J0)

        if callable(self._hess):
            H0 = self._hess(x0, v0)

            if spc.issparse(H0):
                H0 = spc.csr_matrix(H0)
                
                def hess_wrapped(x, v):
                    return spc.csr_matrix(self._hess(x, v))

            elif isinstance(H0, scipy.sparse.linalg.LinearOperator):
                def hess_wrapped(x, v):
                    return self._hess(x, v)

            else:
                H0 = np.atleast_2d(np.asarray(H0))

                def hess_wrapped(x, v):
                    return np.atleast_2d(np.asarray(self._hess(x, v)))

        elif self._hess in ('2-point', '3-point', 'cs'):
            approx_method = self._hess

            def jac_dot_v(x, v):
                J = jac_wrapped(x)
                return J.T.dot(v)

            def hess_wrapped(x, v):
                return approx_derivative(jac_dot_v, x, approx_method,
                                         as_linear_operator=True,
                                         args=(v,))

        else:
            hess_wrapped = self._hess

        self.fun = fun_wrapped
        self.jac = jac_wrapped
        self.hess = hess_wrapped
        self.x0 = x0
        self.f0 = f0
        self.n = x0.size
        self.m = f0.size
        self.kind = _check_kind(self.kind, self.m)
        self.enforce_feasibility \
            = _check_enforce_feasibility(self.enforce_feasibility, self.m)
        if not _is_feasible(self.kind, self.enforce_feasibility, f0):
            raise ValueError("Unfeasible initial point. "
                             "Either set ``enforce_feasibility=False`` or "
                             "choose a new feasible initial point ``x0``.")

        self.isinitialized = True
        return x0


class BoxConstraint:
    """Box constraint.

    Parameters
    ----------
    kind : tuple
        Specifies the type of contraint. Options for this
        parameter are:

            - ('interval', lb, ub) for a constraint of the type:
                lb <= A x <= ub
            - ('greater', lb) for a constraint of the type:
                A x >= lb
            - ('less', ub) for a constraint of the type:
                A x <= ub
            - ('equals', c) for a constraint of the type:
                A x == c
            - ('greater',) for a constraint of the type:
                A x >= 0
            - ('less',) for a constraint of the type:
                A x <= 0
            - ('equals',) for a constraint of the type:
                A x == 0

        where ``lb``,  ``ub`` and ``c`` are (m,) ndarrays or
        scalar values. In the latter case, the same value
        will be repeated for all the constraints.
    enforce_feasibility : {list of boolean, boolean}, optional
        Specify if the constraint must be feasible along the iterations.
        If ``True``  all the iterates generated by the optimization
        algorithm need to be feasible in respect to a constraint. If ``False``
        this is not needed. A list can be passed to specify element-wise
        each constraints needs to stay feasible along the iterations and
        each does not. Alternatively, a single boolean can be used to
        specify the feasibility required of all constraints. By default it
        is False.
    """
    def __init__(self, kind, enforce_feasibility=False):
        self.kind = kind
        self.enforce_feasibility = enforce_feasibility
        self.isinitialized = False

    def evaluate_and_initialize(self, x0, sparse_jacobian=None):
        x0 = np.atleast_1d(x0).astype(float)
        f0 = x0
        self.n = x0.size
        self.m = f0.size
        if sparse_jacobian or sparse_jacobian is None:
            J0 = spc.eye(self.n).tocsr()
            self.sparse_jacobian = True
        else:
            J0 = np.eye(self.n)
            self.sparse_jacobian = False

        self.J0 = J0
        self.kind = _check_kind(self.kind, self.m)
        self.enforce_feasibility \
            = _check_enforce_feasibility(self.enforce_feasibility, self.m)
        self.isinitialized = True
        if not _is_feasible(self.kind, self.enforce_feasibility, f0):
            warn("The initial point was changed in order "
                 "to stay inside box constraints.")
            x0_new = _reinforce_box_constraint(self.kind,
                                               self.enforce_feasibility,
                                               x0)
            self.x0 = x0_new
            self.f0 = x0_new
            return x0_new
        else:
            self.x0 = x0
            self.f0 = f0
            return x0

    def to_linear(self):
        if not self.isinitialized:
            raise RuntimeError("Trying to convert uninitialized constraint.")
        # Build Constraints
        linear = LinearConstraint(self.J0, self.kind,
                                  self.enforce_feasibility)
        linear.isinitialized = True
        linear.m = self.m
        linear.n = self.n
        linear.sparse_jacobian = self.sparse_jacobian
        linear.x0 = self.x0
        linear.f0 = self.f0
        linear.J0 = self.J0
        return linear

    def to_nonlinear(self):
        if not self.isinitialized:
            raise RuntimeError("Trying to convert uninitialized constraint.")
        return self.to_linear().to_nonlinear()
    
def _linear_to_canonical(linear):
    return _nonlinear_to_canonical(linear.to_nonlinear())

def _nonlinear_to_canonical(nonlinear):
    # Parse constraints
    eq, ineq, val_eq, val_ineq, sign, fun_len \
        = _parse_constraint(nonlinear.kind)
    # Get dimensions
    n_eq = len(eq)
    n_ineq = len(ineq)
    n_vars = nonlinear.n

    def new_constr(x):
        c = nonlinear.fun(x)
        return _convert_constr(c, n_vars, n_eq, n_ineq,
                               eq, ineq, val_eq, val_ineq,
                               sign)
    c_ineq0, c_eq0 = _convert_constr(nonlinear.f0, n_vars, n_eq, n_ineq,
                                     eq, ineq, val_eq, val_ineq,
                                     sign)

    if nonlinear.sparse_jacobian:
        def new_jac(x):
            J = nonlinear.jac(x)
            return _convert_sparse_jac(J, n_vars, n_eq, n_ineq,
                                       eq, ineq, val_eq, val_ineq,
                                       sign)
        J_ineq0, J_eq0 = _convert_sparse_jac(nonlinear.J0, n_vars, n_eq,
                                             n_ineq, eq, ineq, val_eq,
                                             val_ineq, sign)

    else:
        def new_jac(x):
            J = nonlinear.jac(x)
            return _convert_dense_jac(J, n_vars, n_eq, n_ineq,
                                      eq, ineq, val_eq, val_ineq,
                                      sign)
        J_ineq0, J_eq0 = _convert_dense_jac(nonlinear.J0, n_vars, n_eq,
                                            n_ineq, eq, ineq, val_eq,
                                            val_ineq, sign)

    if nonlinear.hess is None:
        new_hess = None
    else:
        def new_hess(x, v_eq=np.empty(0), v_ineq=np.empty(0)):
            hess = nonlinear.hess
            v = np.zeros(fun_len)
            if len(v_eq) > 0:
                v[eq] += v_eq
            if len(v_ineq) > 0:
                v[ineq[sign == 1]] += v_ineq[sign == 1]
                v[ineq[sign == -1]] -= v_ineq[sign == -1]
            return hess(x, v)

    if n_ineq == 0:
        enforce_feasibility = np.empty(0, dtype=bool)
    else:
        enforce_feasibility = nonlinear.enforce_feasibility[ineq]

    return CanonicalConstraint(n_vars, n_ineq, n_eq,
                               new_constr, new_jac, new_hess,
                               nonlinear.sparse_jacobian,
                               enforce_feasibility, nonlinear.x0,
                               c_ineq0, c_eq0, J_ineq0, J_eq0)
    
def to_canonical(constraints):
    """Convert constraints or list of constraints to canonical format."""
    # Put ``constraints`` in list format whe needed
    if isinstance(constraints, (NonlinearConstraint,
                                LinearConstraint,
                                BoxConstraint,
                                CanonicalConstraint)):
        constraints = [constraints]
    if isinstance(constraints, (list, tuple, np.array)):
        # Converts all constraints to canonical format
        constraints_list = []
        for c in constraints:
            if isinstance(c, CanonicalConstraint):
                constraints_list += [c]
            elif isinstance(c, (NonlinearConstraint)):
                constraints_list += [_nonlinear_to_canonical(c)]
            elif isinstance(c, (LinearConstraint)):
                constraints_list += [_linear_to_canonical(c)]
            elif isinstance(c, (BoxConstraint)):
                constraints_list += [_box_to_canonical(c)]
            else:
                raise ValueError("Unknown Constraint type.")
        # Concatenate constraints
        if len(constraints_list) == 0:
            raise ValueError("Empty list.")
        elif len(constraints_list) == 1:
            constr = constraints_list[0]
        else:
            constr = _concatenate_canonical_constraints(constraints_list)
    else:
        raise ValueError("Unknown Constraint type.")

    return constr

class BarrierSubproblem:
    """
    Barrier optimization problem:
        minimize fun(x) - barrier_parameter*sum(log(s))
        subject to: constr_eq(x)     = 0
                  constr_ineq(x) + s = 0
    """

    def __init__(self, x0, s0, fun, grad, lagr_hess, n_vars, n_ineq, n_eq,
                 constr, jac, barrier_parameter, tolerance,
                 enforce_feasibility, global_stop_criteria,
                 xtol, fun0, grad0, constr_ineq0, jac_ineq0, constr_eq0,
                 jac_eq0):
        # Store parameters
        self.n_vars = n_vars
        self.x0 = x0
        self.s0 = s0
        self.fun = fun
        self.grad = grad
        self.lagr_hess = lagr_hess
        self.constr = constr
        self.jac = jac
        self.barrier_parameter = barrier_parameter
        self.tolerance = tolerance
        self.n_eq = n_eq
        self.n_ineq = n_ineq
        self.enforce_feasibility = enforce_feasibility
        self.global_stop_criteria = global_stop_criteria
        self.xtol = xtol
        self.fun0 = self._compute_function(fun0, constr_ineq0, s0)
        self.grad0 = self._compute_gradient(grad0)
        self.constr0 = self._compute_constr(constr_ineq0, constr_eq0, s0)
        self.jac0 = self._compute_jacobian(jac_eq0, jac_ineq0, s0)

    def update(self, barrier_parameter, tolerance):
        self.barrier_parameter = barrier_parameter
        self.tolerance = tolerance

    def get_slack(self, z):
        return z[self.n_vars:self.n_vars+self.n_ineq]

    def get_variables(self, z):
        return z[:self.n_vars]

    def function_and_constraints(self, z):
        """Returns barrier function and constraints at given point.

        For z = [x, s], returns barrier function:
            function(z) = fun(x) - barrier_parameter*sum(log(s))
        and barrier constraints:
            constraints(z) = [   constr_eq(x)     ]
                             [ constr_ineq(x) + s ]

        """
        # Get variables and slack variables
        x = self.get_variables(z)
        s = self.get_slack(z)
        # Compute function and constraints
        f = self.fun(x)
        c_ineq, c_eq = self.constr(x)
        # Return objective function and constraints
        return (self._compute_function(f, c_ineq, s),
                self._compute_constr(c_ineq, c_eq, s))

    def _compute_function(self, f, c_ineq, s):
        # Use technique from Nocedal and Wright book, ref [3]_, p.576,
        # to guarantee constraints from `enforce_feasibility`
        # stay feasible along iterations.
        s[self.enforce_feasibility] = -c_ineq[self.enforce_feasibility]
        log_s = [np.log(s_i) if s_i > 0 else -np.inf for s_i in s]
        # Compute barrier objective function
        return f - self.barrier_parameter*np.sum(log_s)

    def _compute_constr(self, c_ineq, c_eq, s):
        # Compute barrier constraint
        return np.hstack((c_eq,
                          c_ineq + s))

    def scaling(self, z):
        """Returns scaling vector.
        Given by:
            scaling = [ones(n_vars), s]
        """
        s = self.get_slack(z)
        diag_elements = np.hstack((np.ones(self.n_vars), s))

        # Diagonal Matrix
        def matvec(vec):
            return diag_elements*vec
        return scipy.sparse.linalg.LinearOperator((self.n_vars+self.n_ineq,
                               self.n_vars+self.n_ineq),
                              matvec)

    def gradient_and_jacobian(self, z):
        """Returns scaled gradient.

        Return scalled gradient:
            gradient = [             grad(x)             ]
                       [ -barrier_parameter*ones(n_ineq) ]
        and scaled Jacobian Matrix:
            jacobian = [  jac_eq(x)  0  ]
                       [ jac_ineq(x) S  ]
        Both of them scaled by the previously defined scaling factor.
        """
        # Get variables and slack variables
        x = self.get_variables(z)
        s = self.get_slack(z)
        # Compute first derivatives
        g = self.grad(x)
        J_ineq, J_eq = self.jac(x)
        # Return gradient and jacobian
        return (self._compute_gradient(g),
                self._compute_jacobian(J_eq, J_ineq, s))

    def _compute_gradient(self, g):
        return np.hstack((g, -self.barrier_parameter*np.ones(self.n_ineq)))

    def _compute_jacobian(self, J_eq, J_ineq, s):
        if self.n_ineq == 0:
            return J_eq
        else:
            if spc.issparse(J_eq) or spc.issparse(J_ineq):
                # It is expected that J_eq and J_ineq
                # are already `csr_matrix` because of
                # the way ``BoxConstraint``, ``NonlinearConstraint``
                # and ``LinearConstraint`` are defined.
                J_eq = spc.csr_matrix(J_eq)
                J_ineq = spc.csr_matrix(J_ineq)
                return self._assemble_sparse_jacobian(J_eq, J_ineq, s)
            else:
                S = np.diag(s)
                zeros = np.zeros((self.n_eq, self.n_ineq))
                # Convert to matrix
                if spc.issparse(J_ineq):
                    J_ineq = J_ineq.toarray()
                if spc.issparse(J_eq):
                    J_eq = J_eq.toarray()
                # Concatenate matrices
                return np.asarray(np.bmat([[J_eq, zeros],
                                           [J_ineq, S]]))

    def _assemble_sparse_jacobian(self, J_eq, J_ineq, s):
        """Assemble sparse jacobian given its components.

        Given ``J_eq``, ``J_ineq`` and ``s`` returns:
            jacobian = [ J_eq,     0     ]
                       [ J_ineq, diag(s) ]

        It is equivalent to:
            spc.bmat([[ J_eq,   None    ],
                      [ J_ineq, diag(s) ]], "csr")
        but significantly more efficient for this
        given structure.
        """
        n_vars, n_ineq, n_eq = self.n_vars, self.n_ineq, self.n_eq
        J_aux = spc.vstack([J_eq, J_ineq], "csr")
        indptr, indices, data = J_aux.indptr, J_aux.indices, J_aux.data
        new_indptr = indptr + np.hstack((np.zeros(n_eq, dtype=int),
                                         np.arange(n_ineq+1, dtype=int)))
        size = indices.size+n_ineq
        new_indices = np.empty(size)
        new_data = np.empty(size)
        mask = np.full(size, False, bool)
        mask[new_indptr[-n_ineq:]-1] = True
        new_indices[mask] = n_vars+np.arange(n_ineq)
        new_indices[~mask] = indices
        new_data[mask] = s
        new_data[~mask] = data
        J = spc.csr_matrix((new_data, new_indices, new_indptr),
                           (n_eq + n_ineq, n_vars + n_ineq))
        return J

    def lagrangian_hessian_x(self, z, v):
        """Returns Lagrangian Hessian (in relation to `x`) -> Hx"""
        x = self.get_variables(z)
        # Get lagrange multipliers relatated to nonlinear equality constraints
        v_eq = v[:self.n_eq]
        # Get lagrange multipliers relatated to nonlinear ineq. constraints
        v_ineq = v[self.n_eq:self.n_eq+self.n_ineq]
        lagr_hess = self.lagr_hess
        return lagr_hess(x, v_eq, v_ineq)

    def lagrangian_hessian_s(self, z, v):
        """Returns scaled Lagrangian Hessian (in relation to`s`) -> S Hs S"""
        s = self.get_slack(z)
        # Using the primal formulation:
        #     S Hs S = diag(s)*diag(barrier_parameter/s**2)*diag(s).
        # Reference [1]_ p. 882, formula (3.1)
        primal = self.barrier_parameter
        # Using the primal-dual formulation
        #     S Hs S = diag(s)*diag(v/s)*diag(s)
        # Reference [1]_ p. 883, formula (3.11)
        primal_dual = v[-self.n_ineq:]*s
        # Uses the primal-dual formulation for
        # positives values of v_ineq, and primal
        # formulation for the remaining ones.
        return np.where(v[-self.n_ineq:] > 0, primal_dual, primal)

    def lagrangian_hessian(self, z, v):
        """Returns scaled Lagrangian Hessian"""
        # Compute Hessian in relation to x and s
        Hx = self.lagrangian_hessian_x(z, v)
        if self.n_ineq > 0:
            S_Hs_S = self.lagrangian_hessian_s(z, v)

        # The scaled Lagragian Hessian is:
        #     [ Hx    0    ]
        #     [ 0   S Hs S ]
        def matvec(vec):
            vec_x = self.get_variables(vec)
            vec_s = self.get_slack(vec)
            if self.n_ineq > 0:
                return np.hstack((Hx.dot(vec_x), S_Hs_S*vec_s))
            else:
                return Hx.dot(vec_x)
        return scipy.sparse.linalg.LinearOperator((self.n_vars+self.n_ineq,
                               self.n_vars+self.n_ineq),
                              matvec)

    def stop_criteria(self, state):
        """Stop criteria to the barrier problem.
        The criteria here proposed is similar to formula (2.3)
        from [1]_, p.879.
        """
        return (state.optimality < self.tolerance
                and state.constr_violation < self.tolerance) \
            or self.global_stop_criteria(state) \
            or state.trust_radius < self.xtol

def equality_constrained_sqp(fun_and_constr, grad_and_jac, lagr_hess,
                             x0, fun0, grad0, constr0,
                             jac0, stop_criteria, state,
                             trust_lb=None,
                             trust_ub=None,
                             initial_penalty=1.0,
                             initial_trust_radius=1.0,
                             scaling=default_scaling,
                             return_all=False,
                             factorization_method=None):
    """Solve nonlinear equality-constrained problem using trust-region SQP.

    Solve optimization problem:

        minimize fun(x)
        subject to: constr(x) = 0

    using Byrd-Omojokun Trust-Region SQP method described in [1]_. Several
    implementation details are based on [2]_ and [3]_, p. 549.

    References
    ----------
    .. [1] Lalee, Marucha, Jorge Nocedal, and Todd Plantenga. "On the
           implementation of an algorithm for large-scale equality
           constrained optimization." SIAM Journal on
           Optimization 8.3 (1998): 682-706.
    .. [2] Byrd, Richard H., Mary E. Hribar, and Jorge Nocedal.
           "An interior point algorithm for large-scale nonlinear
           programming." SIAM Journal on Optimization 9.4 (1999): 877-900.
    .. [3] Nocedal, Jorge, and Stephen J. Wright. "Numerical optimization"
           Second Edition (2006).
    """
    PENALTY_FACTOR = 0.3  # Rho from formula (3.51), reference [2]_, p.891.
    LARGE_REDUCTION_RATIO = 0.9
    INTERMEDIARY_REDUCTION_RATIO = 0.3
    SUFFICIENT_REDUCTION_RATIO = 1e-8  # Eta from reference [2]_, p.892.
    TRUST_ENLARGEMENT_FACTOR_L = 7.0
    TRUST_ENLARGEMENT_FACTOR_S = 2.0
    MAX_TRUST_REDUCTION = 0.5
    MIN_TRUST_REDUCTION = 0.1
    SOC_THRESHOLD = 0.1
    TR_FACTOR = 0.8  # Zeta from formula (3.21), reference [2]_, p.885.
    BOX_FACTOR = 0.5

    n, = np.shape(x0)  # Number of parameters

    # Set default lower and upper bounds.
    if trust_lb is None:
        trust_lb = np.full(n, -np.inf)
    if trust_ub is None:
        trust_ub = np.full(n, np.inf)

    # Initial values
    x = np.copy(x0)
    trust_radius = initial_trust_radius
    penalty = initial_penalty
    # Compute Values
    f = fun0
    c = grad0
    b = constr0
    A = jac0
    S = scaling(x)
    # Get projections
    Z, LS, Y = projections(A, factorization_method)
    # Compute least-square lagrange multipliers
    v = -LS.dot(c)

    # Update state parameters
    state.optimality = norm(c + A.T.dot(v), np.inf)
    state.constr_violation = norm(b, np.inf) if len(b) > 0 else 0
    state.niter += 1
    state.x = x
    state.v = v
    state.fun = f
    state.grad = c
    state.constr = b
    state.jac = A
    state.trust_radius = trust_radius
    state.penalty = penalty
    if return_all:
        state.allvecs += [np.copy(x)]
        state.allmult += [np.copy(v)]

    compute_hess = True
    while not stop_criteria(state):
        # Compute Lagrangian Hessian
        if compute_hess:
            H = lagr_hess(x, v)
            state.nhev += 1

        # Normal Step - `dn`
        # minimize 1/2*||A dn + b||^2
        # subject to:
        # ||dn|| <= TR_FACTOR * trust_radius
        # BOX_FACTOR * lb <= dn <= BOX_FACTOR * ub.
        dn = modified_dogleg(A, Y, b,
                             TR_FACTOR*trust_radius,
                             BOX_FACTOR*trust_lb,
                             BOX_FACTOR*trust_ub)

        # Tangential Step - `dn`
        # Solve the QP problem:
        # minimize 1/2 dt.T H dt + dt.T (H dn + c)
        # subject to:
        # A dt = 0
        # ||dt|| <= sqrt(trust_radius**2 - ||dn||**2)
        # lb - dn <= dt <= ub - dn
        c_t = H.dot(dn) + c
        b_t = np.zeros_like(b)
        trust_radius_t = np.sqrt(trust_radius**2 - np.linalg.norm(dn)**2)
        lb_t = trust_lb - dn
        ub_t = trust_ub - dn
        dt, info_cg = projected_cg(H, c_t, Z, Y, b_t,
                                   trust_radius_t,
                                   lb_t, ub_t)

        # Compute update (normal + tangential steps).
        d = dn + dt

        # Compute second order model: 1/2 d H d + c.T d + f.
        quadratic_model = 1/2*(H.dot(d)).dot(d) + c.T.dot(d)
        # Compute linearized constraint: l = A d + b.
        linearized_constr = A.dot(d)+b
        # Compute new penalty parameter according to formula (3.52),
        # reference [2]_, p.891.
        vpred = norm(b) - norm(linearized_constr)
        # Guarantee `vpred` always positive,
        # regardless of roundoff errors.
        vpred = max(1e-16, vpred)
        previous_penalty = penalty
        if quadratic_model > 0:
            new_penalty = quadratic_model / ((1-PENALTY_FACTOR)*vpred)
            penalty = max(penalty, new_penalty)
        # Compute predicted reduction according to formula (3.52),
        # reference [2]_, p.891.
        predicted_reduction = -quadratic_model + penalty*vpred

        # Compute merit function at current point
        merit_function = f + penalty*norm(b)
        # Evaluate function and constraints at trial point
        x_next = x + S.dot(d)
        f_next, b_next = fun_and_constr(x_next)
        # Increment funcion evaluation counter
        state.nfev += 1
        state.ncev += 1
        # Compute merit function at trial point
        merit_function_next = f_next + penalty*norm(b_next)
        # Compute actual reduction according to formula (3.54),
        # reference [2]_, p.892.
        actual_reduction = merit_function - merit_function_next
        # Compute reduction ratio
        reduction_ratio = actual_reduction / predicted_reduction

        # Second order correction (SOC), reference [2]_, p.892.
        if reduction_ratio < SUFFICIENT_REDUCTION_RATIO and \
           norm(dn) <= SOC_THRESHOLD * norm(dt):
            # Compute second order correction
            y = -Y.dot(b_next)
            # Make sure increment is inside box constraints
            _, t, intersect = box_intersections(d, y, trust_lb, trust_ub)
            # Compute tentative point
            x_soc = x + S.dot(d + t*y)
            f_soc, b_soc = fun_and_constr(x_soc)
            # Increment funcion evaluation counter
            state.nfev += 1
            state.ncev += 1
            # Recompute actual reduction
            merit_function_soc = f_soc + penalty*norm(b_soc)
            actual_reduction_soc = merit_function - merit_function_soc
            # Recompute reduction ratio
            reduction_ratio_soc = actual_reduction_soc / predicted_reduction
            if intersect and reduction_ratio_soc >= SUFFICIENT_REDUCTION_RATIO:
                x_next = x_soc
                f_next = f_soc
                b_next = b_soc
                reduction_ratio = reduction_ratio_soc

        # Readjust trust region step, formula (3.55), reference [2]_, p.892.
        if reduction_ratio >= LARGE_REDUCTION_RATIO:
            trust_radius = max(TRUST_ENLARGEMENT_FACTOR_L * norm(d),
                               trust_radius)
        elif reduction_ratio >= INTERMEDIARY_REDUCTION_RATIO:
            trust_radius = max(TRUST_ENLARGEMENT_FACTOR_S * norm(d),
                               trust_radius)
        # Reduce trust region step, according to reference [3]_, p.696.
        elif reduction_ratio < SUFFICIENT_REDUCTION_RATIO:
                trust_reduction \
                    = (1-SUFFICIENT_REDUCTION_RATIO)/(1-reduction_ratio)
                new_trust_radius = trust_reduction * norm(d)
                if new_trust_radius >= MAX_TRUST_REDUCTION * trust_radius:
                    trust_radius *= MAX_TRUST_REDUCTION
                elif new_trust_radius >= MIN_TRUST_REDUCTION * trust_radius:
                    trust_radius = new_trust_radius
                else:
                    trust_radius *= MIN_TRUST_REDUCTION

        # Update iteration
        state.niter += 1
        if reduction_ratio >= SUFFICIENT_REDUCTION_RATIO:
            x = x_next
            f, b = f_next, b_next
            c, A = grad_and_jac(x)
            S = scaling(x)
            # Increment funcion evaluation counter
            state.ngev += 1
            state.njev += 1
            # Get projections
            Z, LS, Y = projections(A)
            # Compute least-square lagrange multipliers
            v = -LS.dot(c)
            # Set Flag
            compute_hess = True
            # Store state
            state.x = x
            state.v = v
            state.fun = f
            state.grad = c
            state.constr = b
            state.jac = A
            # Otimality values
            state.optimality = norm(c + A.T.dot(v), np.inf)
            state.constr_violation = norm(b, np.inf) if len(b) > 0 else 0
        else:
            penalty = previous_penalty
            compute_hess = False
        # Store values
        state.trust_radius = trust_radius
        state.penalty = penalty
        state.cg_niter += info_cg["niter"]
        state.cg_info = info_cg
        if return_all:
            state.allvecs.append(np.copy(x))
            state.allmult.append(np.copy(v))

    return state

def default_scaling(x):
    n, = np.shape(x)
    return spc.eye(n)


def tr_interior_point(fun, grad, lagr_hess, n_vars, n_ineq, n_eq,
                      constr, jac, x0, fun0, grad0,
                      constr_ineq0, jac_ineq0, constr_eq0,
                      jac_eq0, stop_criteria,
                      enforce_feasibility, xtol, state,
                      initial_barrier_parameter=0.1,
                      initial_tolerance=0.1,
                      initial_penalty=1.0,
                      initial_trust_radius=1.0,
                      return_all=False,
                      factorization_method=None):
    """Trust-region interior points method.

    Solve problem:
        minimize fun(x)
        subject to: constr_ineq(x) <= 0
                    constr_eq(x) = 0
    using trust-region interior point method described in [1]_.
    """
    # BOUNDARY_PARAMETER controls the decrease on the slack
    # variables. Represents ``tau`` from [1]_ p.885, formula (3.18).
    BOUNDARY_PARAMETER = 0.995
    # BARRIER_DECAY_RATIO controls the decay of the barrier parameter
    # and of the subproblem toloerance. Represents ``theta`` from [1]_ p.879.
    BARRIER_DECAY_RATIO = 0.2
    # TRUST_ENLARGEMENT controls the enlargement on trust radius
    # after each iteration
    TRUST_ENLARGEMENT = 5

    # Default enforce_feasibility
    if enforce_feasibility is None:
        enforce_feasibility = np.zeros(n_ineq, bool)
    # Initial Values
    state.barrier_parameter = initial_barrier_parameter
    state.tolerance = initial_tolerance
    state.trust_radius = initial_trust_radius
    state.penalty = initial_penalty
    state.optimality = np.inf
    state.constr_violation = np.inf
    # Define initial value for the slack variables
    s0 = np.maximum(-1.5*constr_ineq0, np.ones(n_ineq))
    # Define barrier subproblem
    subprob = BarrierSubproblem(
        x0, s0, fun, grad, lagr_hess, n_vars, n_ineq, n_eq, constr, jac,
        state.barrier_parameter, state.tolerance, enforce_feasibility,
        stop_criteria, xtol, fun0, grad0, constr_ineq0, jac_ineq0,
        constr_eq0, jac_eq0)
    # Define initial parameter for the first iteration.
    z = np.hstack((x0, s0))
    fun0_subprob, constr0_subprob = subprob.fun0, subprob.constr0
    grad0_subprob, jac0_subprob = subprob.grad0, subprob.jac0
    # Define trust region bounds
    trust_lb = np.hstack((np.full(subprob.n_vars, -np.inf),
                          np.full(subprob.n_ineq, -BOUNDARY_PARAMETER)))
    trust_ub = np.full(subprob.n_vars+subprob.n_ineq, np.inf)

    # If there are inequality constraints solve a
    # sequence of barrier problems
    first_barrier_prob = True
    while True:
        if not first_barrier_prob:
            # Update parameters
            state.trust_radius = max(initial_trust_radius,
                                     TRUST_ENLARGEMENT*state.trust_radius)
            # TODO: Use more advanced strategies from [2]_
            # to update this parameters.
            state.barrier_parameter *= BARRIER_DECAY_RATIO
            state.tolerance *= BARRIER_DECAY_RATIO
        first_barrier_prob = False
        # Update Barrier Problem
        subprob.update(state.barrier_parameter, state.tolerance)
        # Solve SQP subproblem
        state = equality_constrained_sqp(
            subprob.function_and_constraints,
            subprob.gradient_and_jacobian,
            subprob.lagrangian_hessian,
            z, fun0_subprob, grad0_subprob,
            constr0_subprob, jac0_subprob, subprob.stop_criteria,
            state, trust_lb, trust_ub, initial_penalty,
            state.trust_radius, subprob.scaling, return_all,
            factorization_method)
        z = state.x
        if stop_criteria(state):
            break
        # Compute initial values for next iteration
        fun0_subprob, constr0_subprob = subprob.function_and_constraints(z)
        grad0_subprob, jac0_subprob = subprob.gradient_and_jacobian(z)

    # Get x and s
    state.x = subprob.get_variables(z)
    state.s = subprob.get_slack(z)
    # Return all
    if return_all:
        allvecs = []
        allslack = []
        for z in state.allvecs:
            allvecs += [subprob.get_variables(z)]
            allslack += [subprob.get_slack(z)]
        state.allvecs = allvecs
        state.allslack = allslack

    return state

def minimize_constrained(fun, x0, grad, hess='2-point', constraints=(),
                         method=None, xtol=1e-8, gtol=1e-8,
                         sparse_jacobian=None, options={},
                         callback=None, max_iter=1000,
                         verbose=0):
    # Initial value
    x0 = np.atleast_1d(x0).astype(float)
    n_vars = np.size(x0)

    f0 = fun(x0)
    g0 = np.atleast_1d(grad(x0))

    def grad_wrapped(x):
        return np.atleast_1d(grad(x))

    H0 = hess(x0)
    H0 = np.atleast_2d(np.asarray(H0))
    
    def hess_wrapped(x):
        return np.atleast_2d(np.asarray(hess(x)))

    # Put constraints in list format when needed
    if isinstance(constraints, (NonlinearConstraint,
                                LinearConstraint,
                                BoxConstraint)):
        constraints = [constraints]
    # Copy, evaluate and initialize constraints
    copied_constraints = [deepcopy(constr) for constr in constraints]
    for constr in copied_constraints:
        x0 = constr.evaluate_and_initialize(x0, sparse_jacobian)
    # Concatenate constraints
    if len(copied_constraints) == 0:
        constr = empty_canonical_constraint(x0, n_vars, sparse_jacobian)
    else:
        constr = to_canonical(copied_constraints)

    # Generate Lagrangian hess function
    lagr_hess = lagrangian_hessian(constr, hess_wrapped)

    state = OptimizeResult(niter=0, nfev=1, ngev=1,
                           ncev=1, njev=1, nhev=0,
                           cg_niter=0, cg_info={})
    # Store values
    return_all = options.get("return_all", False)
    if return_all:
        state.allvecs = []
        state.allmult = []
        
    def stop_criteria(state):
        barrier_tol = options.get("barrier_tol", 1e-8)
        state.status = None
        if (callback is not None) and callback(state):
            state.status = 3
        elif state.optimality < gtol and state.constr_violation < gtol:
            state.status = 1
        elif (state.trust_radius < xtol
              and state.barrier_parameter < barrier_tol):
            state.status = 2
        elif state.niter > max_iter:
            state.status = 0
        return state.status in (0, 1, 2, 3)

    start_time = time.time()
    # Call inferior function to do the optimization
    if constr.n_ineq == 0:
        warn("The problem only has equality constraints. "
             "The solver 'equality_constrained_sqp' is a "
             "better choice for those situations.")
    result = tr_interior_point(
        fun, grad_wrapped, lagr_hess,
        n_vars, constr.n_ineq, constr.n_eq,
        constr.constr, constr.jac,
        x0, f0, g0, constr.c_ineq0, constr.J_ineq0,
        constr.c_eq0, constr.J_eq0, stop_criteria,
        constr.enforce_feasibility,
        xtol, state, **options)

    def fun_and_constr(x):
        f = fun(x)
        _, c_eq = constr.constr(x)
        return f, c_eq

    def grad_and_jac(x):
        g = grad_wrapped(x)
        _, J_eq = constr.jac(x)
        return g, J_eq

    result = equality_constrained_sqp(
        fun_and_constr, grad_and_jac, lagr_hess,
        x0, f0, g0, constr.c_eq0, constr.J_eq0,
        stop_criteria, state, **options)


    result.execution_time = time.time() - start_time
    result.method = method
    result.message = TERMINATION_MESSAGES[result.status]

    return result
    
    
class CanonicalConstraint:
    def __init__(self, n_vars, n_ineq, n_eq,
                 constr, jac, hess, sparse_jacobian,
                 enforce_feasibility,
                 x0, c_ineq0, c_eq0, J_ineq0, J_eq0):
        # Dimensions
        self.n_vars = n_vars
        self.n_ineq = n_ineq
        self.n_eq = n_eq
        # Objective function and constraints
        self.constr = constr
        self.jac = jac
        self.hess = hess
        # Use sparse jacobian flag
        self.sparse_jacobian = sparse_jacobian
        # Enforce feasibility for CanonicalConstraint. Should
        # be a list of booleans (and never a single boolean value,
        # as it is allowed for Box, Linear and Nonlinear constraints).
        self.enforce_feasibility = enforce_feasibility
        # Initial Values
        self.x0 = x0
        self.c_ineq0 = c_ineq0
        self.c_eq0 = c_eq0
        self.J_ineq0 = J_ineq0
        self.J_eq0 = J_eq0



def lagrangian_hessian(constraint, hess):

    # Concatenate Hessians
    def lagr_hess(x, v_eq=np.empty(0), v_ineq=np.empty(0)):
        n = len(x)
        hess_list = []
        if hess is not None:
            hess_list += [hess(x)]
        if constraint.hess is not None:
            hess_list += [constraint.hess(x, v_eq, v_ineq)]

        def matvec(p):
            result = np.zeros_like(p)
            for h in hess_list:
                result += h.dot(p)
            return result

        return spc.linalg.LinearOperator((n, n), matvec)

    return lagr_hess


def empty_canonical_constraint(x0, n_vars, sparse_jacobian=None):
    n_eq = 0
    n_ineq = 0
    empty_c = np.empty(0)
    if sparse_jacobian or (sparse_jacobian is None):
        empty_J = spc.csr_matrix(np.empty((0, n_vars)))
    else:
        empty_J = np.empty((0, n_vars))

    def constr(x):
        return empty_c, empty_c

    def jac(x):
        return empty_J, empty_J

    enforce_feasibility = np.empty(0, dtype=bool)
    return CanonicalConstraint(n_vars, n_ineq, n_eq,
                               constr, jac, None,
                               True, enforce_feasibility,
                               x0, empty_c, empty_c,
                               empty_J, empty_J)



def orthogonality(A, g):
    # Compute vector norms
    norm_g = np.linalg.norm(g)
    # Compute Frobenius norm of the matrix A
    if issparse(A):
        norm_A = scipy.sparse.linalg.norm(A, ord='fro')
    else:
        norm_A = np.linalg.norm(A, ord='fro')

    # Check if norms are zero
    if norm_g == 0 or norm_A == 0:
        return 0

    norm_A_g = np.linalg.norm(A.dot(g))
    # Orthogonality measure
    orth = norm_A_g / (norm_A*norm_g)
    return orth

    # z = inv(A A.T) A x
    def least_squares(x):
        return factor(A.dot(x))

    # z = A.T inv(A A.T) x
    def row_space(x):
        return A.T.dot(factor(x))

    return null_space, least_squares, row_space


def augmented_system_projections(A, m, n, orth_tol, max_refin, tol):
    K = csc_matrix(bmat([[eye(n), A.T], [A, None]]))
    solve = scipy.sparse.linalg.factorized(K)

    def null_space(x):
        v = np.hstack([x, np.zeros(m)])
        lu_sol = solve(v)
        z = lu_sol[:n]
        k = 0
        while orthogonality(A, z) > orth_tol:
            if k >= max_refin:
                break
            new_v = v - K.dot(lu_sol)
            lu_update = solve(new_v)
            lu_sol += lu_update
            z = lu_sol[:n]
            k += 1

        return z

    def least_squares(x):
        v = np.hstack([x, np.zeros(m)])
        lu_sol = solve(v)
        # return z = inv(A A.T) A x
        return lu_sol[n:m+n]

    def row_space(x):
        v = np.hstack([np.zeros(n), x])
        lu_sol = solve(v)
        return lu_sol[:n]

    return null_space, least_squares, row_space



def projections(A, method=None, orth_tol=1e-12, max_refin=3, tol=1e-15):
    m, n = np.shape(A)

    # The factorization of an empty matrix
    # only works for the sparse representation.
    if m*n == 0:
        A = csc_matrix(A)

    null_space, least_squares, row_space \
        = augmented_system_projections(A, m, n, orth_tol, max_refin, tol)

    Z = spc.linalg.LinearOperator((n, n), null_space)
    LS = spc.linalg.LinearOperator((m, n), least_squares)
    Y = spc.linalg.LinearOperator((n, m), row_space)

    return Z, LS, Y


def sphere_intersections(z, d, trust_radius,
                         entire_line=False):

    # Special case when d=0
    if norm(d) == 0:
        return 0, 0, False
    # Check for inf trust_radius
    if np.isinf(trust_radius):
        if entire_line:
            ta = -np.inf
            tb = np.inf
        else:
            ta = 0
            tb = 1
        intersect = True
        return ta, tb, intersect

    a = np.dot(d, d)
    b = 2 * np.dot(z, d)
    c = np.dot(z, z) - trust_radius**2
    discriminant = b*b - 4*a*c
    if discriminant < 0:
        intersect = False
        return 0, 0, intersect
    sqrt_discriminant = np.sqrt(discriminant)

    aux = b + copysign(sqrt_discriminant, b)
    ta = -aux / (2*a)
    tb = -2*c / aux
    ta, tb = sorted([ta, tb])

    if entire_line:
        intersect = True
    else:
        # Checks to see if intersection happens
        # within vectors length.
        if tb < 0 or ta > 1:
            intersect = False
            ta = 0
            tb = 0
        else:
            intersect = True
            # Restrict intersection interval
            # between 0 and 1.
            ta = max(0, ta)
            tb = min(1, tb)

    return ta, tb, intersect


def box_intersections(z, d, lb, ub,
                      entire_line=False):

    # Make sure it is a numpy array
    z = np.asarray(z)
    d = np.asarray(d)
    lb = np.asarray(lb)
    ub = np.asarray(ub)
    # Special case when d=0
    if norm(d) == 0:
        return 0, 0, False

    # Get values for which d==0
    zero_d = (d == 0)
    # If the boundaries are not satisfied for some coordinate
    # for which "d" is zero, there is no box-line intersection.
    if (z[zero_d] < lb[zero_d]).any() or (z[zero_d] > ub[zero_d]).any():
        intersect = False
        return 0, 0, intersect
    # Remove values for which d is zero
    not_zero_d = np.logical_not(zero_d)
    z = z[not_zero_d]
    d = d[not_zero_d]
    lb = lb[not_zero_d]
    ub = ub[not_zero_d]

    # Find a series of intervals (t_lb[i], t_ub[i]).
    t_lb = (lb-z) / d
    t_ub = (ub-z) / d
    # Get the intersection of all those intervals.
    ta = max(np.minimum(t_lb, t_ub))
    tb = min(np.maximum(t_lb, t_ub))

    # Check if intersection is feasible
    if ta <= tb:
        intersect = True
    else:
        intersect = False
    # Checks to see if intersection happens within vectors length.
    if not entire_line:
        if tb < 0 or ta > 1:
            intersect = False
            ta = 0
            tb = 0
        else:
            # Restrict intersection interval between 0 and 1.
            ta = max(0, ta)
            tb = min(1, tb)

    return ta, tb, intersect


def box_sphere_intersections(z, d, lb, ub, trust_radius,
                             entire_line=False,
                             extra_info=False):

    ta_b, tb_b, intersect_b = box_intersections(z, d, lb, ub, # 
                                                entire_line)
    ta_s, tb_s, intersect_s = sphere_intersections(z, d,
                                                   trust_radius,
                                                   entire_line)
    ta = np.maximum(ta_b, ta_s)
    tb = np.minimum(tb_b, tb_s)
    if intersect_b and intersect_s and ta <= tb:
        intersect = True
    else:
        intersect = False

    if extra_info:
        sphere_info = {'ta': ta_s, 'tb': tb_s, 'intersect': intersect_s}
        box_info = {'ta': ta_b, 'tb': tb_b, 'intersect': intersect_b}
        return ta, tb, intersect, sphere_info, box_info
    else:
        return ta, tb, intersect


def inside_box_boundaries(x, lb, ub):
    """Check if lb <= x <= ub."""
    return (lb <= x).all() and (x <= ub).all()

def reinforce_box_boundaries(x, lb, ub):
    """Return clipped value of x"""
    return np.minimum(np.maximum(x, lb), ub)


def reinforce_box_boundaries(x, lb, ub):
    """Return clipped value of x"""
    return np.minimum(np.maximum(x, lb), ub)


def modified_dogleg(A, Y, b, trust_radius, lb, ub):
    # Compute minimum norm minimizer of 1/2*|| A x + b ||^2.
    newton_point = -Y.dot(b)
    # Check for interior point
    if inside_box_boundaries(newton_point, lb, ub)  \
       and norm(newton_point) <= trust_radius:
        x = newton_point
        return x

    # Compute gradient vector ``g = A.T b``
    g = A.T.dot(b)
    # Compute cauchy point
    # `cauchy_point = g.T g / (g.T A.T A g)``.
    A_g = A.dot(g)
    cauchy_point = -np.dot(g, g) / np.dot(A_g, A_g) * g
    # Origin
    origin_point = np.zeros_like(cauchy_point)

    z = cauchy_point
    p = newton_point - cauchy_point
    _, alpha, intersect = box_sphere_intersections(z, p, lb, ub,
                                                   trust_radius)
    if intersect:
        x1 = z + alpha*p
    else:
        z = origin_point
        p = cauchy_point
        _, alpha, _ = box_sphere_intersections(z, p, lb, ub,
                                               trust_radius)
        x1 = z + alpha*p

    z = origin_point
    p = newton_point
    _, alpha, _ = box_sphere_intersections(z, p, lb, ub,
                                           trust_radius)
    x2 = z + alpha*p

    if norm(A.dot(x1) + b) < norm(A.dot(x2) + b):
        return x1
    else:
        return x2


def projected_cg(H, c, Z, Y, b, trust_radius=np.inf,
                 lb=None, ub=None, tol=None,
                 max_iter=None, max_infeasible_iter=None,
                 return_all=False):
    CLOSE_TO_ZERO = 1e-25

    n, = np.shape(c)  # Number of parameters
    m, = np.shape(b)  # Number of constraints

    x = Y.dot(-b)
    r = Z.dot(H.dot(x) + c)
    g = Z.dot(r)
    p = -g

    if return_all:
        allvecs = [x]
    H_p = H.dot(p)
    rt_g = norm(g)**2  # g.T g = r.T Z g = r.T g (ref [1]_ p.1389)

    tr_distance = trust_radius - norm(x)
    if tr_distance < 0:
        raise ValueError("Trust region problem does not have a solution.")
    elif tr_distance < CLOSE_TO_ZERO:
        info = {'niter': 0, 'stop_cond': 2, 'hits_boundary': True}
        if return_all:
            allvecs.append(x)
            info['allvecs'] = allvecs
        return x, info

    if tol is None:
        tol = max(min(0.01 * np.sqrt(rt_g), 0.1 * rt_g), CLOSE_TO_ZERO)
    if lb is None:
        lb = np.full(n, -np.inf)
    if ub is None:
        ub = np.full(n, np.inf)
    if max_iter is None:
        max_iter = n-m
    max_iter = min(max_iter, n-m)
    if max_infeasible_iter is None:
        max_infeasible_iter = n-m

    hits_boundary = False
    stop_cond = 1
    counter = 0
    last_feasible_x = np.empty_like(x)
    k = 0
    for i in range(max_iter):
        # Stop criteria - Tolerance : r.T g < tol
        if rt_g < tol:
            stop_cond = 4
            break
        k += 1
        # Compute curvature
        pt_H_p = H_p.dot(p)
        # Stop criteria - Negative curvature
        if pt_H_p <= 0:
            if np.isinf(trust_radius):
                    raise ValueError("Negative curvature not "
                                     "allowed for unrestrited "
                                     "problems.")
            else:
                _, alpha, intersect = box_sphere_intersections(
                    x, p, lb, ub, trust_radius, entire_line=True)
                if intersect:
                    x = x + alpha*p
                x = reinforce_box_boundaries(x, lb, ub)
                stop_cond = 3
                hits_boundary = True
                break

        alpha = rt_g / pt_H_p
        x_next = x + alpha*p

        if np.linalg.norm(x_next) >= trust_radius:
            _, theta, intersect = box_sphere_intersections(x, alpha*p, lb, ub,
                                                           trust_radius)
            if intersect:
                x = x + theta*alpha*p
            x = reinforce_box_boundaries(x, lb, ub)
            stop_cond = 2
            hits_boundary = True
            break

        if inside_box_boundaries(x_next, lb, ub):
            counter = 0
        else:
            counter += 1
        if counter > 0:
            _, theta, intersect = box_sphere_intersections(x, alpha*p, lb, ub,
                                                           trust_radius)
            if intersect:
                last_feasible_x = x + theta*alpha*p
                last_feasible_x = reinforce_box_boundaries(last_feasible_x,
                                                           lb, ub)
                counter = 0
        if counter > max_infeasible_iter:
            break
        if return_all:
            allvecs.append(x_next)

        r_next = r + alpha*H_p
        # Project residual g+ = Z r+
        g_next = Z.dot(r_next)
        # Compute conjugate direction step d
        rt_g_next = norm(g_next)**2  # g.T g = r.T g (ref [1]_ p.1389)
        beta = rt_g_next / rt_g
        p = - g_next + beta*p
        # Prepare for next iteration
        x = x_next
        g = g_next
        r = g_next
        rt_g = norm(g)**2  # g.T g = r.T Z g = r.T g (ref [1]_ p.1389)
        H_p = H.dot(p)

    if not inside_box_boundaries(x, lb, ub):
        x = last_feasible_x
        hits_boundary = True
    info = {'niter': k, 'stop_cond': stop_cond,
            'hits_boundary': hits_boundary}
    if return_all:
        info['allvecs'] = allvecs
    return x, info


if __name__ == "__main__":
    prob = Rosenbrock()
    result = minimize_constrained(prob.fun, prob.x0,
                                  prob.grad, prob.hess,
                                  prob.constr)
 
    print (result)
