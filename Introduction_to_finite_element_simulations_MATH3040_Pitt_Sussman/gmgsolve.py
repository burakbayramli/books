import numpy as np
import scipy.linalg as la


def vcycle(A,f):
    # perform one v-cycle on the matrix A

    sizeF = np.size(A,axis=0);

    # directSize=size for direct inversion
    if sizeF < 15:
        v = la.solve(A,f)
        return v

    # N1=number of Gauss-Seidel iterations before coarsening
    N1 = 5;
    v = np.zeros(sizeF);
    for numGS in range(N1):
        for k in range(sizeF):
            v[k] = (f[k] - np.dot(A[k,0:k], v[0:k]) \
                     -np.dot(A[k,k+1:], v[k+1:]) ) / A[k,k];
    
    # construct interpolation operator from next coarser to this mesh
    # next coarser has ((n-1)/2 + 1 ) points
    assert(sizeF%2 ==1)
    sizeC =  (sizeF-1)/2 +1
    P = np.zeros((sizeF,sizeC));
    for k in range(sizeC):
        P[2*k,k] = 1;   # copy these points
    for k in range(sizeC-1):
        P[2*k+1,k] = .5;    # average these points
        P[2*k+1,k+1] = .5;

    # compute residual
    residual = f - np.dot(A,v)

    # project residual onto coarser mesh
    residC = np.dot(P.transpose(),residual)

    # Find coarser matrix  (sizeC X sizeC)
    AC = np.dot(P.transpose(),np.dot(A,P))

    vC = vcycle(AC,residC);

    # extend to this mesh
    v = np.dot(P,vC)

    # N2=number of Gauss-Seidel iterations after coarsening
    N2 = 5;
    for numGS in range(N2):
        for k in range(sizeF):
            v[k] = (f[k] - np.dot(A[k,0:k], v[0:k]) \
                     -np.dot(A[k,k+1:], v[k+1:]) ) / A[k,k];
    return v


N = 2**11+1
x = np.linspace(0,1,N);
h = x[1]-x[0]

# tridiagonal matrix
A = np.diag(2.*np.ones(N)) - np.diag(np.ones(N-1),  1) \
                           - np.diag(np.ones(N-1), -1)
A = A/h**2

f = np.ones(N, dtype=float)  #rhs

udirect = la.solve(A, f)  # correct solution

u = np.zeros(N) # initial guess
for iters in range(100):
    r = f - np.dot(A,u)
    if la.norm(r)/la.norm(f) < 1.e-10:
      break
    du = vcycle(A, r)
    u += du

    print "step %d, rel error=%e"% \
        (iters+1, la.norm(u-udirect)/la.norm(udirect) )



