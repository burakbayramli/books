from math import copysign
import numpy as np
import itertools, time
from numpy.linalg import norm
from warnings import warn
from scipy.linalg import get_blas_funcs
import scipy.sparse as sps
import scipy.sparse.linalg

from math import copysign
import numpy as np
import operator

EPS = np.finfo(np.float64).eps

def _adjust_scheme_to_bounds(x0, h, num_steps, scheme, lb, ub):
    if scheme == '1-sided':
        use_one_sided = np.ones_like(h, dtype=bool)
    elif scheme == '2-sided':
        h = np.abs(h)
        use_one_sided = np.zeros_like(h, dtype=bool)
    else:
        raise ValueError("`scheme` must be '1-sided' or '2-sided'.")

    if np.all((lb == -np.inf) & (ub == np.inf)):
        return h, use_one_sided

    h_total = h * num_steps
    h_adjusted = h.copy()

    lower_dist = x0 - lb
    upper_dist = ub - x0

    if scheme == '1-sided':
        x = x0 + h_total
        violated = (x < lb) | (x > ub)
        fitting = np.abs(h_total) <= np.maximum(lower_dist, upper_dist)
        h_adjusted[violated & fitting] *= -1

        forward = (upper_dist >= lower_dist) & ~fitting
        h_adjusted[forward] = upper_dist[forward] / num_steps
        backward = (upper_dist < lower_dist) & ~fitting
        h_adjusted[backward] = -lower_dist[backward] / num_steps
    elif scheme == '2-sided':
        central = (lower_dist >= h_total) & (upper_dist >= h_total)

        forward = (upper_dist >= lower_dist) & ~central
        h_adjusted[forward] = np.minimum(
            h[forward], 0.5 * upper_dist[forward] / num_steps)
        use_one_sided[forward] = True

        backward = (upper_dist < lower_dist) & ~central
        h_adjusted[backward] = -np.minimum(
            h[backward], 0.5 * lower_dist[backward] / num_steps)
        use_one_sided[backward] = True

        min_dist = np.minimum(upper_dist, lower_dist) / num_steps
        adjusted_central = (~central & (np.abs(h_adjusted) <= min_dist))
        h_adjusted[adjusted_central] = min_dist[adjusted_central]
        use_one_sided[adjusted_central] = False

    return h_adjusted, use_one_sided


relative_step = {"2-point": EPS**0.5,
                 "3-point": EPS**(1/3),
                 "cs": EPS**0.5}


def _compute_absolute_step(rel_step, x0, method):
    if rel_step is None:
        rel_step = relative_step[method]
    sign_x0 = (x0 >= 0).astype(float) * 2 - 1
    return rel_step * sign_x0 * np.maximum(1.0, np.abs(x0))


def _dense_difference(fun, x0, f0, h, use_one_sided, method):
    m = f0.size
    n = x0.size
    J_transposed = np.empty((n, m))
    h_vecs = np.diag(h)

    for i in range(h.size):
        if method == '2-point':
            x = x0 + h_vecs[i]
            dx = x[i] - x0[i]  # Recompute dx as exactly representable number.
            df = fun(x) - f0
        elif method == '3-point' and use_one_sided[i]:
            x1 = x0 + h_vecs[i]
            x2 = x0 + 2 * h_vecs[i]
            dx = x2[i] - x0[i]
            f1 = fun(x1)
            f2 = fun(x2)
            df = -3.0 * f0 + 4 * f1 - f2
        elif method == '3-point' and not use_one_sided[i]:
            x1 = x0 - h_vecs[i]
            x2 = x0 + h_vecs[i]
            dx = x2[i] - x1[i]
            f1 = fun(x1)
            f2 = fun(x2)
            df = f2 - f1
        elif method == 'cs':
            f1 = fun(x0 + h_vecs[i]*1.j)
            df = f1.imag
            dx = h_vecs[i, i]
        else:
            raise RuntimeError("Never be here.")

        J_transposed[i] = df / dx

    if m == 1:
        J_transposed = np.ravel(J_transposed)

    return J_transposed.T


def _prepare_bounds(bounds, x0):
    lb, ub = [np.asarray(b, dtype=float) for b in bounds]
    if lb.ndim == 0:
        lb = np.resize(lb, x0.shape)

    if ub.ndim == 0:
        ub = np.resize(ub, x0.shape)

    return lb, ub


def approx_derivative(fun, x0, method='3-point', rel_step=None, f0=None,
                      bounds=(-np.inf, np.inf), sparsity=None,
                      as_linear_operator=False, args=(), kwargs={}):
    if method not in ['2-point', '3-point', 'cs']:
        raise ValueError("Unknown method '%s'. " % method)

    x0 = np.atleast_1d(x0)
    if x0.ndim > 1:
        raise ValueError("`x0` must have at most 1 dimension.")

    lb, ub = _prepare_bounds(bounds, x0)

    if lb.shape != x0.shape or ub.shape != x0.shape:
        raise ValueError("Inconsistent shapes between bounds and `x0`.")

    if as_linear_operator and not (np.all(np.isinf(lb))
                                   and np.all(np.isinf(ub))):
        raise ValueError("Bounds not supported when "
                         "`as_linear_operator` is True.")

    def fun_wrapped(x):
        f = np.atleast_1d(fun(x, *args, **kwargs))
        if f.ndim > 1:
            raise RuntimeError("`fun` return value has "
                               "more than 1 dimension.")
        return f

    if f0 is None:
        f0 = fun_wrapped(x0)
    else:
        f0 = np.atleast_1d(f0)
        if f0.ndim > 1:
            raise ValueError("`f0` passed has more than 1 dimension.")

    if np.any((x0 < lb) | (x0 > ub)):
        raise ValueError("`x0` violates bound constraints.")

    if as_linear_operator:
        if rel_step is None:
            rel_step = relative_step[method]

        return _linear_operator_difference(fun_wrapped, x0,
                                           f0, rel_step, method)
    else:
        h = _compute_absolute_step(rel_step, x0, method)

        if method == '2-point':
            h, use_one_sided = _adjust_scheme_to_bounds(
                x0, h, 1, '1-sided', lb, ub)
        elif method == '3-point':
            h, use_one_sided = _adjust_scheme_to_bounds(
                x0, h, 1, '2-sided', lb, ub)
        elif method == 'cs':
            use_one_sided = False

        if sparsity is None:
            return _dense_difference(fun_wrapped, x0, f0, h,
                                     use_one_sided, method)
        else:
            if not issparse(sparsity) and len(sparsity) == 2:
                structure, groups = sparsity
            else:
                structure = sparsity
                groups = group_columns(sparsity)

            if issparse(structure):
                structure = csc_matrix(structure)
            else:
                structure = np.atleast_2d(structure)

            groups = np.atleast_1d(groups)
            return _sparse_difference(fun_wrapped, x0, f0, h,
                                      use_one_sided, structure,
                                      groups, method)



def isintlike(x):
    """Is x appropriate as an index into a sparse matrix? Returns True
    if it can be cast safely to a machine int.
    """
    # Fast-path check to eliminate non-scalar values. operator.index would
    # catch this case too, but the exception catching is slow.
    if np.ndim(x) != 0:
        return False
    try:
        operator.index(x)
    except (TypeError, ValueError):
        try:
            loose_int = bool(int(x) == x)
        except (TypeError, ValueError):
            return False
        if loose_int:
            warnings.warn("Inexact indices into sparse matrices are deprecated",
                          DeprecationWarning)
        return loose_int
    return True


def isshape(x, nonneg=False):
    """Is x a valid 2-tuple of dimensions?

    If nonneg, also checks that the dimensions are non-negative.
    """
    try:
        # Assume it's a tuple of matrix dimensions (M, N)
        (M, N) = x
    except Exception:
        return False
    else:
        if isintlike(M) and isintlike(N):
            if np.ndim(M) == 0 and np.ndim(N) == 0:
                if not nonneg or (M >= 0 and N >= 0):
                    return True
        return False


class LinearOperator(object):
    def __new__(cls, *args, **kwargs):
        if cls is LinearOperator:
            # Operate as _CustomLinearOperator factory.
            return super(LinearOperator, cls).__new__(_CustomLinearOperator)
        else:
            obj = super(LinearOperator, cls).__new__(cls)

            if (type(obj)._matvec == LinearOperator._matvec
                    and type(obj)._matmat == LinearOperator._matmat):
                warnings.warn("LinearOperator subclass should implement"
                              " at least one of _matvec and _matmat.",
                              category=RuntimeWarning, stacklevel=2)

            return obj

    def __init__(self, dtype, shape):
        """Initialize this LinearOperator.

        To be called by subclasses. ``dtype`` may be None; ``shape`` should
        be convertible to a length-2 tuple.
        """
        if dtype is not None:
            dtype = np.dtype(dtype)

        shape = tuple(shape)
        if not isshape(shape):
            raise ValueError("invalid shape %r (must be 2-d)" % (shape,))

        self.dtype = dtype
        self.shape = shape

    def _init_dtype(self):
        """Called from subclasses at the end of the __init__ routine.
        """
        if self.dtype is None:
            v = np.zeros(self.shape[-1])
            self.dtype = np.asarray(self.matvec(v)).dtype

    def _matmat(self, X):
        """Default matrix-matrix multiplication handler.

        Falls back on the user-defined _matvec method, so defining that will
        define matrix multiplication (though in a very suboptimal way).
        """

        return np.hstack([self.matvec(col.reshape(-1,1)) for col in X.T])

    def _matvec(self, x):
        """Default matrix-vector multiplication handler.

        If self is a linear operator of shape (M, N), then this method will
        be called on a shape (N,) or (N, 1) ndarray, and should return a
        shape (M,) or (M, 1) ndarray.

        This default implementation falls back on _matmat, so defining that
        will define matrix-vector multiplication as well.
        """
        return self.matmat(x.reshape(-1, 1))

    def matvec(self, x):
        """Matrix-vector multiplication.

        Performs the operation y=A*x where A is an MxN linear
        operator and x is a column vector or 1-d array.

        Parameters
        ----------
        x : {matrix, ndarray}
            An array with shape (N,) or (N,1).

        Returns
        -------
        y : {matrix, ndarray}
            A matrix or ndarray with shape (M,) or (M,1) depending
            on the type and shape of the x argument.

        Notes
        -----
        This matvec wraps the user-specified matvec routine or overridden
        _matvec method to ensure that y has the correct shape and type.

        """

        x = np.asanyarray(x)

        M,N = self.shape

        if x.shape != (N,) and x.shape != (N,1):
            raise ValueError('dimension mismatch')

        y = self._matvec(x)

        if isinstance(x, np.matrix):
            y = asmatrix(y)
        else:
            y = np.asarray(y)

        if x.ndim == 1:
            y = y.reshape(M)
        elif x.ndim == 2:
            y = y.reshape(M,1)
        else:
            raise ValueError('invalid shape returned by user-defined matvec()')

        return y

    def rmatvec(self, x):
        """Adjoint matrix-vector multiplication.

        Performs the operation y = A^H * x where A is an MxN linear
        operator and x is a column vector or 1-d array.

        Parameters
        ----------
        x : {matrix, ndarray}
            An array with shape (M,) or (M,1).

        Returns
        -------
        y : {matrix, ndarray}
            A matrix or ndarray with shape (N,) or (N,1) depending
            on the type and shape of the x argument.

        Notes
        -----
        This rmatvec wraps the user-specified rmatvec routine or overridden
        _rmatvec method to ensure that y has the correct shape and type.

        """

        x = np.asanyarray(x)

        M,N = self.shape

        if x.shape != (M,) and x.shape != (M,1):
            raise ValueError('dimension mismatch')

        y = self._rmatvec(x)

        if isinstance(x, np.matrix):
            y = asmatrix(y)
        else:
            y = np.asarray(y)

        if x.ndim == 1:
            y = y.reshape(N)
        elif x.ndim == 2:
            y = y.reshape(N,1)
        else:
            raise ValueError('invalid shape returned by user-defined rmatvec()')

        return y

    def _rmatvec(self, x):
        """Default implementation of _rmatvec; defers to adjoint."""
        if type(self)._adjoint == LinearOperator._adjoint:
            # _adjoint not overridden, prevent infinite recursion
            raise NotImplementedError
        else:
            return self.H.matvec(x)

    def matmat(self, X):
        """Matrix-matrix multiplication.

        Performs the operation y=A*X where A is an MxN linear
        operator and X dense N*K matrix or ndarray.

        Parameters
        ----------
        X : {matrix, ndarray}
            An array with shape (N,K).

        Returns
        -------
        Y : {matrix, ndarray}
            A matrix or ndarray with shape (M,K) depending on
            the type of the X argument.

        Notes
        -----
        This matmat wraps any user-specified matmat routine or overridden
        _matmat method to ensure that y has the correct type.

        """

        X = np.asanyarray(X)

        if X.ndim != 2:
            raise ValueError('expected 2-d ndarray or matrix, not %d-d'
                             % X.ndim)

        M,N = self.shape

        if X.shape[0] != N:
            raise ValueError('dimension mismatch: %r, %r'
                             % (self.shape, X.shape))

        Y = self._matmat(X)

        if isinstance(Y, np.matrix):
            Y = asmatrix(Y)

        return Y

    def __call__(self, x):
        return self*x

    def __mul__(self, x):
        return self.dot(x)

    def dot(self, x):
        """Matrix-matrix or matrix-vector multiplication.

        Parameters
        ----------
        x : array_like
            1-d or 2-d array, representing a vector or matrix.

        Returns
        -------
        Ax : array
            1-d or 2-d array (depending on the shape of x) that represents
            the result of applying this linear operator on x.

        """
        if isinstance(x, LinearOperator):
            return _ProductLinearOperator(self, x)
        elif np.isscalar(x):
            return _ScaledLinearOperator(self, x)
        else:
            x = np.asarray(x)

            if x.ndim == 1 or x.ndim == 2 and x.shape[1] == 1:
                return self.matvec(x)
            elif x.ndim == 2:
                return self.matmat(x)
            else:
                raise ValueError('expected 1-d or 2-d array or matrix, got %r'
                                 % x)

    def __matmul__(self, other):
        if np.isscalar(other):
            raise ValueError("Scalar operands are not allowed, "
                             "use '*' instead")
        return self.__mul__(other)

    def __rmatmul__(self, other):
        if np.isscalar(other):
            raise ValueError("Scalar operands are not allowed, "
                             "use '*' instead")
        return self.__rmul__(other)

    def __rmul__(self, x):
        if np.isscalar(x):
            return _ScaledLinearOperator(self, x)
        else:
            return NotImplemented

    def __pow__(self, p):
        if np.isscalar(p):
            return _PowerLinearOperator(self, p)
        else:
            return NotImplemented

    def __add__(self, x):
        if isinstance(x, LinearOperator):
            return _SumLinearOperator(self, x)
        else:
            return NotImplemented

    def __neg__(self):
        return _ScaledLinearOperator(self, -1)

    def __sub__(self, x):
        return self.__add__(-x)

    def __repr__(self):
        M,N = self.shape
        if self.dtype is None:
            dt = 'unspecified dtype'
        else:
            dt = 'dtype=' + str(self.dtype)

        return '<%dx%d %s with %s>' % (M, N, self.__class__.__name__, dt)

    def adjoint(self):
        """Hermitian adjoint.

        Returns the Hermitian adjoint of self, aka the Hermitian
        conjugate or Hermitian transpose. For a complex matrix, the
        Hermitian adjoint is equal to the conjugate transpose.

        Can be abbreviated self.H instead of self.adjoint().

        Returns
        -------
        A_H : LinearOperator
            Hermitian adjoint of self.
        """
        return self._adjoint()

    H = property(adjoint)

    def transpose(self):
        """Transpose this linear operator.

        Returns a LinearOperator that represents the transpose of this one.
        Can be abbreviated self.T instead of self.transpose().
        """
        return self._transpose()

    T = property(transpose)

    def _adjoint(self):
        """Default implementation of _adjoint; defers to rmatvec."""
        shape = (self.shape[1], self.shape[0])
        return _CustomLinearOperator(shape, matvec=self.rmatvec,
                                     rmatvec=self.matvec,
                                     dtype=self.dtype)


class _CustomLinearOperator(LinearOperator):
    """Linear operator defined in terms of user-specified operations."""

    def __init__(self, shape, matvec, rmatvec=None, matmat=None, dtype=None):
        super(_CustomLinearOperator, self).__init__(dtype, shape)

        self.args = ()

        self.__matvec_impl = matvec
        self.__rmatvec_impl = rmatvec
        self.__matmat_impl = matmat

        self._init_dtype()

    def _matmat(self, X):
        if self.__matmat_impl is not None:
            return self.__matmat_impl(X)
        else:
            return super(_CustomLinearOperator, self)._matmat(X)

    def _matvec(self, x):
        return self.__matvec_impl(x)

    def _rmatvec(self, x):
        func = self.__rmatvec_impl
        if func is None:
            raise NotImplementedError("rmatvec is not defined")
        return self.__rmatvec_impl(x)

    def _adjoint(self):
        return _CustomLinearOperator(shape=(self.shape[1], self.shape[0]),
                                     matvec=self.__rmatvec_impl,
                                     rmatvec=self.__matvec_impl,
                                     dtype=self.dtype)

    
class OptimizeResult(dict):
    def __getattr__(self, name):
        try:
            return self[name]
        except KeyError:
            raise AttributeError(name)

    __setattr__ = dict.__setitem__
    __delattr__ = dict.__delitem__

    def __repr__(self):
        if self.keys():
            m = max(map(len, list(self.keys()))) + 1
            return '\n'.join([k.rjust(m) + ': ' + repr(v)
                              for k, v in sorted(self.items())])
        else:
            return self.__class__.__name__ + "()"

    def __dir__(self):
        return list(self.keys())



class BFGS:
    _syr = get_blas_funcs('syr', dtype='d')  # Symmetric rank 1 update
    _syr2 = get_blas_funcs('syr2', dtype='d')  # Symmetric rank 2 update
    _symv = get_blas_funcs('symv', dtype='d')
    
    def __init__(self, exception_strategy='skip_update', min_curvature=None,
                 init_scale='auto'):
        if exception_strategy == 'skip_update':
            if min_curvature is not None:
                self.min_curvature = min_curvature
            else:
                self.min_curvature = 1e-8
        elif exception_strategy == 'damp_update':
            if min_curvature is not None:
                self.min_curvature = min_curvature
            else:
                self.min_curvature = 0.2
        else:
            raise ValueError("`exception_strategy` must be 'skip_update' "
                             "or 'damp_update'.")

        self.init_scale = init_scale
        self.first_iteration = None
        self.approx_type = None
        self.B = None
        self.H = None
        self.exception_strategy = exception_strategy

    def initialize(self, n, approx_type):
        self.first_iteration = True
        self.n = n
        self.approx_type = approx_type
        if approx_type not in ('hess', 'inv_hess'):
            raise ValueError("`approx_type` must be 'hess' or 'inv_hess'.")
        # Create matrix
        if self.approx_type == 'hess':
            self.B = np.eye(n, dtype=float)
        else:
            self.H = np.eye(n, dtype=float)

    def _update_inverse_hessian(self, ys, Hy, yHy, s):
        self.H = self._syr2(-1.0 / ys, s, Hy, a=self.H)
        self.H = self._syr((ys+yHy)/ys**2, s, a=self.H)

    def _update_hessian(self, ys, Bs, sBs, y):
        self.B = self._syr(1.0 / ys, y, a=self.B)
        self.B = self._syr(-1.0 / sBs, Bs, a=self.B)
            
    def _auto_scale(self, delta_x, delta_grad):
        s_norm2 = np.dot(delta_x, delta_x)
        y_norm2 = np.dot(delta_grad, delta_grad)
        ys = np.abs(np.dot(delta_grad, delta_x))
        if ys == 0.0 or y_norm2 == 0 or s_norm2 == 0:
            return 1
        if self.approx_type == 'hess':
            return y_norm2 / ys
        else:
            return ys / y_norm2

    def _update_implementation(self, delta_x, delta_grad):
        # Auxiliary variables w and z
        if self.approx_type == 'hess':
            w = delta_x
            z = delta_grad
        else:
            w = delta_grad
            z = delta_x
        wz = np.dot(w, z)
        Mw = self.dot(w)
        wMw = Mw.dot(w)
        if wMw <= 0.0:
            scale = self._auto_scale(delta_x, delta_grad)
            if self.approx_type == 'hess':
                self.B = scale * np.eye(self.n, dtype=float)
            else:
                self.H = scale * np.eye(self.n, dtype=float)
            Mw = self.dot(w)
            wMw = Mw.dot(w)
        if wz <= self.min_curvature * wMw:
            if self.exception_strategy == 'skip_update':
                return
            elif self.exception_strategy == 'damp_update':
                update_factor = (1-self.min_curvature) / (1 - wz/wMw)
                z = update_factor*z + (1-update_factor)*Mw
                wz = np.dot(w, z)
        # Update matrix
        if self.approx_type == 'hess':
            self._update_hessian(wz, Mw, wMw, z)
        else:
            self._update_inverse_hessian(wz, Mw, wMw, z)


    def update(self, delta_x, delta_grad):
        if np.all(delta_x == 0.0):
            return
        if np.all(delta_grad == 0.0):
            warn('delta_grad == 0.0', UserWarning)
            return
        if self.first_iteration:
            if self.init_scale == "auto":
                scale = self._auto_scale(delta_x, delta_grad)
            else:
                scale = float(self.init_scale)
            if self.approx_type == 'hess':
                self.B *= scale
            else:
                self.H *= scale
            self.first_iteration = False
        self._update_implementation(delta_x, delta_grad)

    def dot(self, p):
        if self.approx_type == 'hess':
            return self._symv(1, self.B, p)
        else:
            return self._symv(1, self.H, p)

    def get_matrix(self):
        if self.approx_type == 'hess':
            M = np.copy(self.B)
        else:
            M = np.copy(self.H)
        li = np.tril_indices_from(M, k=-1)
        M[li] = M.T[li]
        return M

class NonlinearConstraint(object):
    def __init__(self, fun, lb, ub, jac='2-point', hess=BFGS(),
                 keep_feasible=False, finite_diff_rel_step=None,
                 finite_diff_jac_sparsity=None):
        self.fun = fun
        self.lb = lb
        self.ub = ub
        self.finite_diff_rel_step = finite_diff_rel_step
        self.finite_diff_jac_sparsity = finite_diff_jac_sparsity
        self.jac = jac
        self.hess = hess
        self.keep_feasible = keep_feasible


class LinearConstraint(object):
    def __init__(self, A, lb, ub, keep_feasible=False):
        self.A = A
        self.lb = lb
        self.ub = ub
        self.keep_feasible = keep_feasible



class Bounds(object):
    def __init__(self, lb, ub, keep_feasible=False):
        self.lb = lb
        self.ub = ub
        self.keep_feasible = keep_feasible

    def __repr__(self):
        if np.any(self.keep_feasible):
            return "{}({!r}, {!r}, keep_feasible={!r})".format(type(self).__name__, self.lb, self.ub, self.keep_feasible)
        else:
            return "{}({!r}, {!r})".format(type(self).__name__, self.lb, self.ub)
    
class CanonicalConstraint(object):

    def __init__(self, n_eq, n_ineq, fun, jac, hess, keep_feasible):
        self.n_eq = n_eq
        self.n_ineq = n_ineq
        self.fun = fun
        self.jac = jac
        self.hess = hess
        self.keep_feasible = keep_feasible

    @classmethod
    def from_PreparedConstraint(cls, constraint):
        """Create an instance from `PreparedConstrained` object."""
        lb, ub = constraint.bounds
        cfun = constraint.fun
        keep_feasible = constraint.keep_feasible

        if np.all(lb == -np.inf) and np.all(ub == np.inf):
            return cls.empty(cfun.n)

        if np.all(lb == -np.inf) and np.all(ub == np.inf):
            return cls.empty(cfun.n)
        elif np.all(lb == ub):
            return cls._equal_to_canonical(cfun, lb)
        elif np.all(lb == -np.inf):
            return cls._less_to_canonical(cfun, ub, keep_feasible)
        elif np.all(ub == np.inf):
            return cls._greater_to_canonical(cfun, lb, keep_feasible)
        else:
            return cls._interval_to_canonical(cfun, lb, ub, keep_feasible)

    @classmethod
    def empty(cls, n):
        empty_fun = np.empty(0)
        empty_jac = np.empty((0, n))
        empty_hess = sps.csr_matrix((n, n))

        def fun(x):
            return empty_fun, empty_fun

        def jac(x):
            return empty_jac, empty_jac

        def hess(x, v_eq, v_ineq):
            return empty_hess

        return cls(0, 0, fun, jac, hess, np.empty(0, dtype=np.bool))

    @classmethod
    def _interval_to_canonical(cls, cfun, lb, ub, keep_feasible):
        lb_inf = lb == -np.inf
        ub_inf = ub == np.inf
        equal = lb == ub
        less = lb_inf & ~ub_inf
        greater = ub_inf & ~lb_inf
        interval = ~equal & ~lb_inf & ~ub_inf

        equal = np.nonzero(equal)[0]
        less = np.nonzero(less)[0]
        greater = np.nonzero(greater)[0]
        interval = np.nonzero(interval)[0]
        n_less = less.shape[0]
        n_greater = greater.shape[0]
        n_interval = interval.shape[0]
        n_ineq = n_less + n_greater + 2 * n_interval
        n_eq = equal.shape[0]

        keep_feasible = np.hstack((keep_feasible[less],
                                   keep_feasible[greater],
                                   keep_feasible[interval],
                                   keep_feasible[interval]))

        def fun(x):
            f = cfun.fun(x)
            eq = f[equal] - lb[equal]
            le = f[less] - ub[less]
            ge = lb[greater] - f[greater]
            il = f[interval] - ub[interval]
            ig = lb[interval] - f[interval]
            return eq, np.hstack((le, ge, il, ig))

        def jac(x):
            J = cfun.jac(x)
            eq = J[equal]
            le = J[less]
            ge = -J[greater]
            il = J[interval]
            ig = -il
            if sps.issparse(J):
                ineq = sps.vstack((le, ge, il, ig))
            else:
                ineq = np.vstack((le, ge, il, ig))
            return eq, ineq

        def hess(x, v_eq, v_ineq):
            n_start = 0
            v_l = v_ineq[n_start:n_start + n_less]
            n_start += n_less
            v_g = v_ineq[n_start:n_start + n_greater]
            n_start += n_greater
            v_il = v_ineq[n_start:n_start + n_interval]
            n_start += n_interval
            v_ig = v_ineq[n_start:n_start + n_interval]

            v = np.zeros_like(lb)
            v[equal] = v_eq
            v[less] = v_l
            v[greater] = -v_g
            v[interval] = v_il - v_ig

            return cfun.hess(x, v)

        return cls(n_eq, n_ineq, fun, jac, hess, keep_feasible)


class LagrangianHessian(object):
    def __init__(self, n, objective_hess, constraints_hess):
        self.n = n
        self.objective_hess = objective_hess
        self.constraints_hess = constraints_hess

    def __call__(self, x, v_eq=np.empty(0), v_ineq=np.empty(0)):
        H_objective = self.objective_hess(x)
        H_constraints = self.constraints_hess(x, v_eq, v_ineq)

        def matvec(p):
            return H_objective.dot(p) + H_constraints.dot(p)

        return LinearOperator((self.n, self.n), matvec)

class LinearVectorFunction(object):
    def __init__(self, A, x0, sparse_jacobian):
        if sparse_jacobian or sparse_jacobian is None and sps.issparse(A):
            self.J = sps.csr_matrix(A)
            self.sparse_jacobian = True
        elif sps.issparse(A):
            self.J = A.toarray()
            self.sparse_jacobian = False
        else:
            self.J = np.atleast_2d(A)
            self.sparse_jacobian = False

        self.m, self.n = self.J.shape

        self.x = np.atleast_1d(x0).astype(float)
        self.f = self.J.dot(self.x)
        self.f_updated = True

        self.v = np.zeros(self.m, dtype=float)
        self.H = sps.csr_matrix((self.n, self.n))

    def _update_x(self, x):
        if not np.array_equal(x, self.x):
            self.x = np.atleast_1d(x).astype(float)
            self.f_updated = False

    def fun(self, x):
        self._update_x(x)
        if not self.f_updated:
            self.f = self.J.dot(x)
            self.f_updated = True
        return self.f

    def jac(self, x):
        self._update_x(x)
        return self.J

    def hess(self, x, v):
        self._update_x(x)
        self.v = v
        return self.H


class IdentityVectorFunction(LinearVectorFunction):
    def __init__(self, x0, sparse_jacobian):
        n = len(x0)
        if sparse_jacobian or sparse_jacobian is None:
            A = sps.eye(n, format='csr')
            sparse_jacobian = True
        else:
            A = np.eye(n)
            sparse_jacobian = False
        super(IdentityVectorFunction, self).__init__(A, x0, sparse_jacobian)


        
class PreparedConstraint(object):

    def __init__(self, constraint, x0, sparse_jacobian=None,
                 finite_diff_bounds=(-np.inf, np.inf)):
        fun = IdentityVectorFunction(x0, sparse_jacobian)

        m = fun.m
        lb = np.asarray(constraint.lb, dtype=float)
        ub = np.asarray(constraint.ub, dtype=float)
        if lb.ndim == 0:
            lb = np.resize(lb, m)
        if ub.ndim == 0:
            ub = np.resize(ub, m)

        keep_feasible = np.asarray(constraint.keep_feasible, dtype=bool)
        if keep_feasible.ndim == 0:
            keep_feasible = np.resize(keep_feasible, m)
        if keep_feasible.shape != (m,):
            raise ValueError("`keep_feasible` has a wrong shape.")

        mask = keep_feasible & (lb != ub)
        f0 = fun.f
        if np.any(f0[mask] < lb[mask]) or np.any(f0[mask] > ub[mask]):
            raise ValueError("`x0` is infeasible with respect to some "
                             "inequality constraint with `keep_feasible` "
                             "set to True.")

        self.fun = fun
        self.bounds = (lb, ub)
        self.keep_feasible = keep_feasible


FD_METHODS = ('2-point', '3-point', 'cs')

class ScalarFunction(object):

    def __init__(self, fun, x0, args, grad, hess, finite_diff_rel_step,
                 finite_diff_bounds):

        self.x = np.atleast_1d(x0).astype(float)
        self.n = self.x.size
        self.nfev = 0
        self.ngev = 0
        self.nhev = 0
        self.f_updated = False
        self.g_updated = False
        self.H_updated = False

        finite_diff_options = {}
        finite_diff_options["method"] = grad
        finite_diff_options["rel_step"] = finite_diff_rel_step
        finite_diff_options["bounds"] = finite_diff_bounds

        # Function evaluation
        def fun_wrapped(x):
            self.nfev += 1
            return fun(x, *args)

        def update_fun():
            self.f = fun_wrapped(self.x)

        self._update_fun_impl = update_fun
        self._update_fun()

        if grad in FD_METHODS:
            def update_grad():
                self._update_fun()
                self.g = approx_derivative(fun_wrapped, self.x, f0=self.f,
                                           **finite_diff_options)

        self._update_grad_impl = update_grad
        self._update_grad()

        self.H = hess
        self.H.initialize(self.n, 'hess')
        self.H_updated = True
        self.x_prev = None
        self.g_prev = None

        def update_hess():
            self._update_grad()
            self.H.update(self.x - self.x_prev, self.g - self.g_prev)

        self._update_hess_impl = update_hess

        def update_x(x):
            self._update_grad()
            self.x_prev = self.x
            self.g_prev = self.g

            self.x = np.atleast_1d(x).astype(float)
            self.f_updated = False
            self.g_updated = False
            self.H_updated = False
            self._update_hess()

        self._update_x_impl = update_x

    def _update_fun(self):
        if not self.f_updated:
            self._update_fun_impl()
            self.f_updated = True

    def _update_grad(self):
        if not self.g_updated:
            self._update_grad_impl()
            self.g_updated = True

    def _update_hess(self):
        if not self.H_updated:
            self._update_hess_impl()
            self.H_updated = True

    def fun(self, x):
        if not np.array_equal(x, self.x):
            self._update_x_impl(x)
        self._update_fun()
        return self.f

    def grad(self, x):
        if not np.array_equal(x, self.x):
            self._update_x_impl(x)
        self._update_grad()
        return self.g

    def hess(self, x):
        if not np.array_equal(x, self.x):
            self._update_x_impl(x)
        self._update_hess()
        return self.H

class BarrierSubproblem:
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
        self.terminate = False

    def update(self, barrier_parameter, tolerance):
        self.barrier_parameter = barrier_parameter
        self.tolerance = tolerance

    def get_slack(self, z):
        return z[self.n_vars:self.n_vars+self.n_ineq]

    def get_variables(self, z):
        return z[:self.n_vars]

    def function_and_constraints(self, z):
        x = self.get_variables(z)
        s = self.get_slack(z)
        f = self.fun(x)
        c_eq, c_ineq = self.constr(x)
        return (self._compute_function(f, c_ineq, s),
                self._compute_constr(c_ineq, c_eq, s))

    def _compute_function(self, f, c_ineq, s):
        s[self.enforce_feasibility] = -c_ineq[self.enforce_feasibility]
        log_s = [np.log(s_i) if s_i > 0 else -np.inf for s_i in s]
        return f - self.barrier_parameter*np.sum(log_s)

    def _compute_constr(self, c_ineq, c_eq, s):
        return np.hstack((c_eq,
                          c_ineq + s))

    def scaling(self, z):
        s = self.get_slack(z)
        diag_elements = np.hstack((np.ones(self.n_vars), s))

        # Diagonal Matrix
        def matvec(vec):
            return diag_elements*vec
        return LinearOperator((self.n_vars+self.n_ineq,
                               self.n_vars+self.n_ineq),
                              matvec)

    def gradient_and_jacobian(self, z):
        x = self.get_variables(z)
        s = self.get_slack(z)
        g = self.grad(x)
        J_eq, J_ineq = self.jac(x)
        return (self._compute_gradient(g),
                self._compute_jacobian(J_eq, J_ineq, s))

    def _compute_gradient(self, g):
        return np.hstack((g, -self.barrier_parameter*np.ones(self.n_ineq)))

    def _compute_jacobian(self, J_eq, J_ineq, s):
        if self.n_ineq == 0:
            return J_eq
        else:
            if sps.issparse(J_eq) or sps.issparse(J_ineq):
                J_eq = sps.csr_matrix(J_eq)
                J_ineq = sps.csr_matrix(J_ineq)
                return self._assemble_sparse_jacobian(J_eq, J_ineq, s)
            else:
                S = np.diag(s)
                zeros = np.zeros((self.n_eq, self.n_ineq))
                if sps.issparse(J_ineq):
                    J_ineq = J_ineq.toarray()
                if sps.issparse(J_eq):
                    J_eq = J_eq.toarray()
                return np.block([[J_eq, zeros],
                                 [J_ineq, S]])

    def _assemble_sparse_jacobian(self, J_eq, J_ineq, s):

        n_vars, n_ineq, n_eq = self.n_vars, self.n_ineq, self.n_eq
        J_aux = sps.vstack([J_eq, J_ineq], "csr")
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
        J = sps.csr_matrix((new_data, new_indices, new_indptr),
                           (n_eq + n_ineq, n_vars + n_ineq))
        return J

    def lagrangian_hessian_x(self, z, v):
        x = self.get_variables(z)
        v_eq = v[:self.n_eq]
        v_ineq = v[self.n_eq:self.n_eq+self.n_ineq]
        lagr_hess = self.lagr_hess
        return lagr_hess(x, v_eq, v_ineq)

    def lagrangian_hessian_s(self, z, v):
        s = self.get_slack(z)
        primal = self.barrier_parameter
        primal_dual = v[-self.n_ineq:]*s
        return np.where(v[-self.n_ineq:] > 0, primal_dual, primal)

    def lagrangian_hessian(self, z, v):
        Hx = self.lagrangian_hessian_x(z, v)
        if self.n_ineq > 0:
            S_Hs_S = self.lagrangian_hessian_s(z, v)
            
        def matvec(vec):
            vec_x = self.get_variables(vec)
            vec_s = self.get_slack(vec)
            if self.n_ineq > 0:
                return np.hstack((Hx.dot(vec_x), S_Hs_S*vec_s))
            else:
                return Hx.dot(vec_x)
        return LinearOperator((self.n_vars+self.n_ineq,
                               self.n_vars+self.n_ineq),
                              matvec)

    def stop_criteria(self, state, z, last_iteration_failed,
                      optimality, constr_violation,
                      trust_radius, penalty, cg_info):

        x = self.get_variables(z)
        if self.global_stop_criteria(state, x,
                                     last_iteration_failed,
                                     trust_radius, penalty,
                                     cg_info,
                                     self.barrier_parameter,
                                     self.tolerance):
            self.terminate = True
            return True
        else:
            g_cond = (optimality < self.tolerance and
                      constr_violation < self.tolerance)
            x_cond = trust_radius < self.xtol
            return g_cond or x_cond
        
def inside_box_boundaries(x, lb, ub):
    return (lb <= x).all() and (x <= ub).all()

def box_sphere_intersections(z, d, lb, ub, trust_radius,
                             entire_line=False,
                             extra_info=False):

    ta_b, tb_b, intersect_b = box_intersections(z, d, lb, ub, entire_line)
    ta_s, tb_s, intersect_s = sphere_intersections(z, d, trust_radius, entire_line)
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

def box_intersections(z, d, lb, ub,
                      entire_line=False):

    z = np.asarray(z)
    d = np.asarray(d)
    lb = np.asarray(lb)
    ub = np.asarray(ub)
    # Special case when d=0
    if norm(d) == 0:
        return 0, 0, False

    zero_d = (d == 0)
    if (z[zero_d] < lb[zero_d]).any() or (z[zero_d] > ub[zero_d]).any():
        intersect = False
        return 0, 0, intersect
    not_zero_d = np.logical_not(zero_d)
    z = z[not_zero_d]
    d = d[not_zero_d]
    lb = lb[not_zero_d]
    ub = ub[not_zero_d]

    t_lb = (lb-z) / d
    t_ub = (ub-z) / d
    ta = max(np.minimum(t_lb, t_ub))
    tb = min(np.maximum(t_lb, t_ub))

    if ta <= tb:
        intersect = True
    else:
        intersect = False

    if not entire_line:
        if tb < 0 or ta > 1:
            intersect = False
            ta = 0
            tb = 0
        else:
            ta = max(0, ta)
            tb = min(1, tb)

    return ta, tb, intersect

def sphere_intersections(z, d, trust_radius, entire_line=False):

    if norm(d) == 0:
        return 0, 0, False
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
        if tb < 0 or ta > 1:
            intersect = False
            ta = 0
            tb = 0
        else:
            intersect = True
            ta = max(0, ta)
            tb = min(1, tb)

    return ta, tb, intersect


def strict_bounds(lb, ub, keep_feasible, n_vars):
    strict_lb = np.resize(lb, n_vars).astype(float)
    strict_ub = np.resize(ub, n_vars).astype(float)
    keep_feasible = np.resize(keep_feasible, n_vars)
    strict_lb[~keep_feasible] = -np.inf
    strict_ub[~keep_feasible] = np.inf
    return strict_lb, strict_ub

def orthogonality(A, g):
    norm_g = np.linalg.norm(g)
    if sps.issparse(A):
        norm_A = scipy.sparse.linalg.norm(A, ord='fro')
    else:
        norm_A = np.linalg.norm(A, ord='fro')

    if norm_g == 0 or norm_A == 0:
        return 0

    norm_A_g = np.linalg.norm(A.dot(g))
    orth = norm_A_g / (norm_A*norm_g)
    return orth


        
def augmented_system_projections(A, m, n, orth_tol, max_refin, tol):

    K = sps.csc_matrix(sps.bmat([[sps.eye(n), A.T], [A, None]]))
    try:
        solve = scipy.sparse.linalg.factorized(K)
    except RuntimeError:
        warn("Singular Jacobian matrix. Using dense SVD decomposition to "
             "perform the factorizations.")
        return svd_factorization_projections(A.toarray(), m, n, orth_tol, max_refin, tol)

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
        return lu_sol[n:m+n]

    def row_space(x):
        v = np.hstack([np.zeros(n), x])
        lu_sol = solve(v)
        # return z = A.T inv(A A.T) x
        return lu_sol[:n]

    return null_space, least_squares, row_space

        
def default_scaling(x):
    n, = np.shape(x)
    return speye(n)

def projections(A, method=None, orth_tol=1e-12, max_refin=3, tol=1e-15):

    m, n = np.shape(A)

    if m*n == 0:
        A = sps.csc_matrix(A)

    if sps.issparse(A):
        if method is None:
            method = "AugmentedSystem"
        if method not in ("NormalEquation", "AugmentedSystem"):
            raise ValueError("Method not allowed for sparse matrix.")
        if method == "NormalEquation" and not sksparse_available:
            warnings.warn(("Only accepts 'NormalEquation' option when"
                           " scikit-sparse is available. Using "
                           "'AugmentedSystem' option instead."),
                          ImportWarning)
            method = 'AugmentedSystem'
    else:
        if method is None:
            method = "QRFactorization"
        if method not in ("QRFactorization", "SVDFactorization"):
            raise ValueError("Method not allowed for dense array.")

    null_space, least_squares, row_space \
        = augmented_system_projections(A, m, n, orth_tol, max_refin, tol)

    Z = LinearOperator((n, n), null_space)
    LS = LinearOperator((m, n), least_squares)
    Y = LinearOperator((n, m), row_space)

    return Z, LS, Y


def reinforce_box_boundaries(x, lb, ub):
    """Return clipped value of x"""
    return np.minimum(np.maximum(x, lb), ub)


def update_state_sqp(state, x, last_iteration_failed, objective, prepared_constraints,
                     start_time, tr_radius, constr_penalty, cg_info):
    state.nit += 1
    state.nfev = objective.nfev
    state.njev = objective.ngev
    state.nhev = objective.nhev
    
    state.constr_nfev = [0 for c in prepared_constraints]
    state.constr_njev = [0 for c in prepared_constraints]
    state.constr_nhev = [0 for c in prepared_constraints]

    if not last_iteration_failed:
        state.x = x
        state.fun = objective.f
        state.grad = objective.g
        state.v = [c.fun.v for c in prepared_constraints]
        state.constr = [c.fun.f for c in prepared_constraints]
        state.jac = [c.fun.J for c in prepared_constraints]

        state.lagrangian_grad = np.copy(state.grad)
        for c in prepared_constraints:
            state.lagrangian_grad += c.fun.J.T.dot(c.fun.v)
        state.optimality = np.linalg.norm(state.lagrangian_grad, np.inf)

        state.constr_violation = 0
        for i in range(len(prepared_constraints)):
            lb, ub = prepared_constraints[i].bounds
            c = state.constr[i]
            state.constr_violation = np.max([state.constr_violation,
                                             np.max(lb - c),
                                             np.max(c - ub)])

    state.execution_time = time.time() - start_time
    state.tr_radius = tr_radius
    state.constr_penalty = constr_penalty
    state.cg_niter += cg_info["niter"]
    state.cg_stop_cond = cg_info["stop_cond"]

    return state

def update_state_ip(state, x, last_iteration_failed, objective,
                    prepared_constraints, start_time,
                    tr_radius, constr_penalty, cg_info,
                    barrier_parameter, barrier_tolerance):
    state = update_state_sqp(state, x, last_iteration_failed, objective,
                             prepared_constraints, start_time, tr_radius,
                             constr_penalty, cg_info)
    state.barrier_parameter = barrier_parameter
    state.barrier_tolerance = barrier_tolerance
    return state



def standardize_constraints(constraints, x0, meth):
    """Converts constraints to the form required by the solver."""
    all_constraint_types = (NonlinearConstraint, LinearConstraint, dict)
    new_constraint_types = all_constraint_types[:-1]
    if isinstance(constraints, all_constraint_types):
        constraints = [constraints]
    constraints = list(constraints)  

    if meth == 'trust-constr':
        for i, con in enumerate(constraints):
            if not isinstance(con, new_constraint_types):
                constraints[i] = old_constraint_to_new(i, con)
    else:
        for i, con in enumerate(list(constraints)):
            if isinstance(con, new_constraint_types):
                old_constraints = new_constraint_to_old(con, x0)
                constraints[i] = old_constraints[0]
                constraints.extend(old_constraints[1:])  # appends 1 if present

    return constraints

def initial_constraints_as_canonical(n, prepared_constraints, sparse_jacobian):
    c_eq = []
    c_ineq = []
    J_eq = []
    J_ineq = []

    for c in prepared_constraints:
        f = c.fun.f
        J = c.fun.J
        lb, ub = c.bounds
        if np.all(lb == ub):
            c_eq.append(f - lb)
            J_eq.append(J)
        elif np.all(lb == -np.inf):
            finite_ub = ub < np.inf
            c_ineq.append(f[finite_ub] - ub[finite_ub])
            J_ineq.append(J[finite_ub])
        elif np.all(ub == np.inf):
            finite_lb = lb > -np.inf
            c_ineq.append(lb[finite_lb] - f[finite_lb])
            J_ineq.append(-J[finite_lb])
        else:
            lb_inf = lb == -np.inf
            ub_inf = ub == np.inf
            equal = lb == ub
            less = lb_inf & ~ub_inf
            greater = ub_inf & ~lb_inf
            interval = ~equal & ~lb_inf & ~ub_inf

            c_eq.append(f[equal] - lb[equal])
            c_ineq.append(f[less] - ub[less])
            c_ineq.append(lb[greater] - f[greater])
            c_ineq.append(f[interval] - ub[interval])
            c_ineq.append(lb[interval] - f[interval])

            J_eq.append(J[equal])
            J_ineq.append(J[less])
            J_ineq.append(-J[greater])
            J_ineq.append(J[interval])
            J_ineq.append(-J[interval])

    c_eq = np.hstack(c_eq) if c_eq else np.empty(0)
    c_ineq = np.hstack(c_ineq) if c_ineq else np.empty(0)

    if sparse_jacobian:
        vstack = sps.vstack
        empty = sps.csr_matrix((0, n))
    else:
        vstack = np.vstack
        empty = np.empty((0, n))

    J_eq = vstack(J_eq) if J_eq else empty
    J_ineq = vstack(J_ineq) if J_ineq else empty

    return c_eq, c_ineq, J_eq, J_ineq

def modified_dogleg(A, Y, b, trust_radius, lb, ub):

    newton_point = -Y.dot(b)
    if inside_box_boundaries(newton_point, lb, ub)  \
       and norm(newton_point) <= trust_radius:
        x = newton_point
        return x

    g = A.T.dot(b)
    A_g = A.dot(g)
    cauchy_point = -np.dot(g, g) / np.dot(A_g, A_g) * g
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
    rt_g = norm(g)**2  

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
    last_feasible_x = np.zeros_like(x)
    k = 0
    for i in range(max_iter):
        if rt_g < tol:
            stop_cond = 4
            break
        k += 1
        pt_H_p = H_p.dot(p)
        if pt_H_p <= 0:
            if np.isinf(trust_radius):
                raise ValueError("Negative curvature not allowed "
                                 "for unrestricted problems.")
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
        g_next = Z.dot(r_next)
        rt_g_next = norm(g_next)**2 
        beta = rt_g_next / rt_g
        p = - g_next + beta*p
        x = x_next
        g = g_next
        r = g_next
        rt_g = norm(g)**2
        H_p = H.dot(p)

    if not inside_box_boundaries(x, lb, ub):
        x = last_feasible_x
        hits_boundary = True
    info = {'niter': k, 'stop_cond': stop_cond,
            'hits_boundary': hits_boundary}
    if return_all:
        info['allvecs'] = allvecs
    return x, info


def equality_constrained_sqp(fun_and_constr, grad_and_jac, lagr_hess,
                             x0, fun0, grad0, constr0,
                             jac0, stop_criteria,
                             state,
                             initial_penalty,
                             initial_trust_radius,
                             factorization_method,
                             trust_lb=None,
                             trust_ub=None,
                             scaling=default_scaling):

    PENALTY_FACTOR = 0.3 
    LARGE_REDUCTION_RATIO = 0.9
    INTERMEDIARY_REDUCTION_RATIO = 0.3
    SUFFICIENT_REDUCTION_RATIO = 1e-8 
    TRUST_ENLARGEMENT_FACTOR_L = 7.0
    TRUST_ENLARGEMENT_FACTOR_S = 2.0
    MAX_TRUST_REDUCTION = 0.5
    MIN_TRUST_REDUCTION = 0.1
    SOC_THRESHOLD = 0.1
    TR_FACTOR = 0.8
    BOX_FACTOR = 0.5
    
    n, = np.shape(x0)  # Number of parameters

    if trust_lb is None:
        trust_lb = np.full(n, -np.inf)
    if trust_ub is None:
        trust_ub = np.full(n, np.inf)

    x = np.copy(x0)
    trust_radius = initial_trust_radius
    penalty = initial_penalty
    f = fun0
    c = grad0
    b = constr0
    A = jac0
    S = scaling(x)
    Z, LS, Y = projections(A, factorization_method)
    v = -LS.dot(c)
    H = lagr_hess(x, v)

    optimality = norm(c + A.T.dot(v), np.inf)
    constr_violation = norm(b, np.inf) if len(b) > 0 else 0
    cg_info = {'niter': 0, 'stop_cond': 0,
               'hits_boundary': False}

    last_iteration_failed = False
    while not stop_criteria(state, x, last_iteration_failed,
                            optimality, constr_violation,
                            trust_radius, penalty, cg_info):
        dn = modified_dogleg(A, Y, b,
                             TR_FACTOR*trust_radius,
                             BOX_FACTOR*trust_lb,
                             BOX_FACTOR*trust_ub)

        c_t = H.dot(dn) + c
        b_t = np.zeros_like(b)
        trust_radius_t = np.sqrt(trust_radius**2 - np.linalg.norm(dn)**2)
        lb_t = trust_lb - dn
        ub_t = trust_ub - dn
        dt, cg_info = projected_cg(H, c_t, Z, Y, b_t,
                                   trust_radius_t,
                                   lb_t, ub_t)

        d = dn + dt
        quadratic_model = 1/2*(H.dot(d)).dot(d) + c.T.dot(d)
        linearized_constr = A.dot(d)+b
        vpred = norm(b) - norm(linearized_constr)
        vpred = max(1e-16, vpred)
        previous_penalty = penalty
        if quadratic_model > 0:
            new_penalty = quadratic_model / ((1-PENALTY_FACTOR)*vpred)
            penalty = max(penalty, new_penalty)
        predicted_reduction = -quadratic_model + penalty*vpred

        merit_function = f + penalty*norm(b)
        x_next = x + S.dot(d)
        f_next, b_next = fun_and_constr(x_next)
        merit_function_next = f_next + penalty*norm(b_next)
        actual_reduction = merit_function - merit_function_next
        reduction_ratio = actual_reduction / predicted_reduction

        if reduction_ratio < SUFFICIENT_REDUCTION_RATIO and \
           norm(dn) <= SOC_THRESHOLD * norm(dt):
            y = -Y.dot(b_next)
            _, t, intersect = box_intersections(d, y, trust_lb, trust_ub)
            x_soc = x + S.dot(d + t*y)
            f_soc, b_soc = fun_and_constr(x_soc)
            merit_function_soc = f_soc + penalty*norm(b_soc)
            actual_reduction_soc = merit_function - merit_function_soc
            reduction_ratio_soc = actual_reduction_soc / predicted_reduction
            if intersect and reduction_ratio_soc >= SUFFICIENT_REDUCTION_RATIO:
                x_next = x_soc
                f_next = f_soc
                b_next = b_soc
                reduction_ratio = reduction_ratio_soc

        if reduction_ratio >= LARGE_REDUCTION_RATIO:
            trust_radius = max(TRUST_ENLARGEMENT_FACTOR_L * norm(d),
                               trust_radius)
        elif reduction_ratio >= INTERMEDIARY_REDUCTION_RATIO:
            trust_radius = max(TRUST_ENLARGEMENT_FACTOR_S * norm(d),
                               trust_radius)
        elif reduction_ratio < SUFFICIENT_REDUCTION_RATIO:
            trust_reduction = ((1-SUFFICIENT_REDUCTION_RATIO) /
                               (1-reduction_ratio))
            new_trust_radius = trust_reduction * norm(d)
            if new_trust_radius >= MAX_TRUST_REDUCTION * trust_radius:
                trust_radius *= MAX_TRUST_REDUCTION
            elif new_trust_radius >= MIN_TRUST_REDUCTION * trust_radius:
                trust_radius = new_trust_radius
            else:
                trust_radius *= MIN_TRUST_REDUCTION

        if reduction_ratio >= SUFFICIENT_REDUCTION_RATIO:
            x = x_next
            f, b = f_next, b_next
            c, A = grad_and_jac(x)
            S = scaling(x)
            Z, LS, Y = projections(A, factorization_method)
            v = -LS.dot(c)
            H = lagr_hess(x, v)
            last_iteration_failed = False
            optimality = norm(c + A.T.dot(v), np.inf)
            constr_violation = norm(b, np.inf) if len(b) > 0 else 0
        else:
            penalty = previous_penalty
            last_iteration_failed = True

    return x, state
        

def tr_interior_point(fun, grad, lagr_hess, n_vars, n_ineq, n_eq,
                      constr, jac, x0, fun0, grad0,
                      constr_ineq0, jac_ineq0, constr_eq0,
                      jac_eq0, stop_criteria,
                      enforce_feasibility, xtol, state,
                      initial_barrier_parameter,
                      initial_tolerance,
                      initial_penalty,
                      initial_trust_radius,
                      factorization_method):
    
    BOUNDARY_PARAMETER = 0.995
    BARRIER_DECAY_RATIO = 0.2
    TRUST_ENLARGEMENT = 5

    if enforce_feasibility is None:
        enforce_feasibility = np.zeros(n_ineq, bool)
    barrier_parameter = initial_barrier_parameter
    tolerance = initial_tolerance
    trust_radius = initial_trust_radius
    s0 = np.maximum(-1.5*constr_ineq0, np.ones(n_ineq))
    subprob = BarrierSubproblem(
        x0, s0, fun, grad, lagr_hess, n_vars, n_ineq, n_eq, constr, jac,
        barrier_parameter, tolerance, enforce_feasibility,
        stop_criteria, xtol, fun0, grad0, constr_ineq0, jac_ineq0,
        constr_eq0, jac_eq0)
    z = np.hstack((x0, s0))
    fun0_subprob, constr0_subprob = subprob.fun0, subprob.constr0
    grad0_subprob, jac0_subprob = subprob.grad0, subprob.jac0
    trust_lb = np.hstack((np.full(subprob.n_vars, -np.inf),
                          np.full(subprob.n_ineq, -BOUNDARY_PARAMETER)))
    trust_ub = np.full(subprob.n_vars+subprob.n_ineq, np.inf)

    while True:
        z, state = equality_constrained_sqp(
            subprob.function_and_constraints,
            subprob.gradient_and_jacobian,
            subprob.lagrangian_hessian,
            z, fun0_subprob, grad0_subprob,
            constr0_subprob, jac0_subprob, subprob.stop_criteria,
            state, initial_penalty, trust_radius,
            factorization_method, trust_lb, trust_ub, subprob.scaling)
        if subprob.terminate:
            break

        trust_radius = max(initial_trust_radius,
                           TRUST_ENLARGEMENT*state.tr_radius)
        barrier_parameter *= BARRIER_DECAY_RATIO
        tolerance *= BARRIER_DECAY_RATIO

        subprob.update(barrier_parameter, tolerance)

        fun0_subprob, constr0_subprob = subprob.function_and_constraints(z)
        grad0_subprob, jac0_subprob = subprob.gradient_and_jacobian(z)

    x = subprob.get_variables(z)
    return x, state

def _minimize_trustregion_constr(fun, x0, args, grad,
                                 hess, hessp, bounds, constraints,
                                 xtol=1e-8, gtol=1e-8,
                                 barrier_tol=1e-8,
                                 sparse_jacobian=None,
                                 callback=None, maxiter=1000,
                                 verbose=0, finite_diff_rel_step=None,
                                 initial_constr_penalty=1.0, initial_tr_radius=1.0,
                                 initial_barrier_parameter=0.1,
                                 initial_barrier_tolerance=0.1,
                                 factorization_method=None,
                                 disp=False):

    x0 = np.atleast_1d(x0).astype(float)
    n_vars = np.size(x0)
    if disp and verbose == 0:
        verbose = 1

    finite_diff_bounds = strict_bounds(bounds.lb, bounds.ub,
                                       bounds.keep_feasible, n_vars)

    objective = ScalarFunction(fun, x0, args, grad, hess,
                               finite_diff_rel_step, finite_diff_bounds)


    # Prepare constraints.
    prepared_constraints = [
        PreparedConstraint(c, x0, sparse_jacobian, finite_diff_bounds)
        for c in constraints]

    n_sparse = sum(c.fun.sparse_jacobian for c in prepared_constraints)
    if 0 < n_sparse < len(prepared_constraints):
        raise ValueError("All constraints must have the same kind of the "
                         "Jacobian --- either all sparse or all dense. "
                         "You can set the sparsity globally by setting "
                         "`sparse_jacobian` to either True of False.")
    if prepared_constraints:
        sparse_jacobian = n_sparse > 0

    if bounds is not None:
        if sparse_jacobian is None:
            sparse_jacobian = True
        prepared_constraints.append(PreparedConstraint(bounds, x0,
                                                       sparse_jacobian))

    c_eq0, c_ineq0, J_eq0, J_ineq0 = initial_constraints_as_canonical(
        n_vars, prepared_constraints, sparse_jacobian)

    canonical_all = [CanonicalConstraint.from_PreparedConstraint(c)
                     for c in prepared_constraints]

    canonical = canonical_all[0]

    lagrangian_hess = LagrangianHessian(n_vars, objective.hess, canonical.hess)

    state = OptimizeResult(
        nit=0, nfev=0, njev=0, nhev=0,
        cg_niter=0, cg_stop_cond=0,
        fun=objective.f, grad=objective.g,
        lagrangian_grad=np.copy(objective.g),
        constr=[c.fun.f for c in prepared_constraints],
        jac=[c.fun.J for c in prepared_constraints],
        constr_nfev=[0 for c in prepared_constraints],
        constr_njev=[0 for c in prepared_constraints],
        constr_nhev=[0 for c in prepared_constraints],
        v=[c.fun.v for c in prepared_constraints],
        method='tr_interior_point')

    start_time = time.time()
    
    def stop_criteria(state, x, last_iteration_failed, tr_radius,
                      constr_penalty, cg_info, barrier_parameter,
                      barrier_tolerance):
        state = update_state_ip(state, x, last_iteration_failed,
                                objective, prepared_constraints,
                                start_time, tr_radius, constr_penalty,
                                cg_info, barrier_parameter, barrier_tolerance)
        state.status = None
        state.niter = state.nit  # Alias for callback (backward-compatibility)
        if callback is not None and callback(np.copy(state.x), state):
            state.status = 3
        elif state.optimality < gtol and state.constr_violation < gtol:
            state.status = 1
        elif (state.tr_radius < xtol
              and state.barrier_parameter < barrier_tol):
            state.status = 2
        elif state.nit > maxiter:
            state.status = 0
        return state.status in (0, 1, 2, 3)

    print ('min TR constr elif tr_interior_point')
    _, result = tr_interior_point(
        objective.fun, objective.grad, lagrangian_hess,
        n_vars, canonical.n_ineq, canonical.n_eq,
        canonical.fun, canonical.jac,
        x0, objective.f, objective.g,
        c_ineq0, J_ineq0, c_eq0, J_eq0,
        stop_criteria,
        canonical.keep_feasible,
        xtol, state, initial_barrier_parameter,
        initial_barrier_tolerance,
        initial_constr_penalty, initial_tr_radius,
        factorization_method)

    result.success = True if result.status in (1, 2) else False
    result.message = 'done'
    result.niter = result.nit

    return result

def minimize(fun, x0, args=(), method=None, jac=None, hess=None,
             hessp=None, bounds=None, constraints=(), tol=None,
             callback=None, options=None):
    meth = 'trust-constr'
    x0 = np.asarray(x0)
    if x0.dtype.kind in np.typecodes["AllInteger"]:
        x0 = np.asarray(x0, dtype=float)
    if not isinstance(args, tuple):
        args = (args,)

    if tol is not None:
        options = dict(options)
        options.setdefault('xtol', tol)
        options.setdefault('gtol', tol)
        options.setdefault('barrier_tol', tol)
        
    constraints = standardize_constraints(constraints, x0, meth)

    return _minimize_trustregion_constr(fun, x0, args, jac, hess, hessp,
                                        bounds, constraints,
                                        callback=callback, **options)


def rosenbrock(x):
    return (1 - x[0])**2 + 100*(x[1] - x[0]**2)**2

x0 = [-1.0,0]

opts = {'maxiter': 1000, 'verbose': 2}

res = minimize (fun=rosenbrock,
                x0=x0,
                method = 'trust-constr',
                jac = "2-point",
                hess = BFGS (),
#                bounds=Bounds([0.3, 0.5], [0.0, 2.0]),
                bounds=Bounds([0.0, 0.0], [3.0, 3.0]),
                options=opts)

print (res)
