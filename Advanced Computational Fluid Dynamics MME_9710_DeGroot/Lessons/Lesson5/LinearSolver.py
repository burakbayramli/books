import numpy as np
from scipy.sparse.linalg import spsolve
from scipy.sparse import csr_matrix

def get_sparse_matrix(PP_coeffs, PU_coeffs, UP_coeffs, UU_coeffs):
    """Function to return a sparse matrix representation of a set of scalar coefficients"""

    # Get number of control volumes (check that all are consistent)
    ncv = PP_coeffs.ncv
    if ncv is not PU_coeffs.ncv or ncv is not UP_coeffs.ncv or ncv is not UU_coeffs.ncv:
        raise ValueError("Not all coefficient arrays have the same dimension")

    # Set up the data and indexing arrays
    nvar = 2  # P, U
    ncoeff = 3  # aW, aP, and aE
    ndata = nvar*nvar*ncoeff*ncv - 2*nvar*nvar # Need to subtract boundary coefficients
    data = np.zeros(ndata)
    rows = np.zeros(ndata, dtype=int)
    cols = np.zeros(ndata, dtype=int)

    # Set up the first cell
    data[0] = PP_coeffs.aP[0]
    cols[0] = 0
    data[1] = PU_coeffs.aP[0]
    cols[1] = 1
    data[2] = PP_coeffs.aE[0]
    cols[2] = 2
    data[3] = PU_coeffs.aE[0]
    cols[3] = 3
    data[4] = UP_coeffs.aP[0]
    cols[4] = 0
    data[5] = UU_coeffs.aP[0]
    cols[5] = 1
    data[6] = UP_coeffs.aE[0]
    cols[6] = 2
    data[7] = UU_coeffs.aE[0]
    cols[7] = 3

    rows[0:4] = 0
    rows[4:8] = 1

    # Set up the interior cells
    for i in range(1, ncv-1):
        start = nvar*nvar*ncoeff*(i-1) + 8
        data[start+0] = PP_coeffs.aW[i]
        cols[start+0] = nvar*i - 2
        data[start+1] = PU_coeffs.aW[i]
        cols[start+1] = nvar*i - 1
        data[start+2] = PP_coeffs.aP[i]
        cols[start+2] = nvar*i
        data[start+3] = PU_coeffs.aP[i]
        cols[start+3] = nvar*i + 1
        data[start+4] = PP_coeffs.aE[i]
        cols[start+4] = nvar*i + 2
        data[start+5] = PU_coeffs.aE[i]
        cols[start+5] = nvar*i + 3
        data[start+6] = UP_coeffs.aW[i]
        cols[start+6] = nvar*i - 2
        data[start+7] = UU_coeffs.aW[i]
        cols[start+7] = nvar*i - 1
        data[start+8] = UP_coeffs.aP[i]
        cols[start+8] = nvar*i
        data[start+9] = UU_coeffs.aP[i]
        cols[start+9] = nvar*i + 1
        data[start+10] = UP_coeffs.aE[i]
        cols[start+10] = nvar*i + 2
        data[start+11] = UU_coeffs.aE[i]
        cols[start+11] = nvar*i + 3

        rows[start:start+6] = nvar*i
        rows[start+6:start+12] = nvar*i + 1

    # Set up the last cell
    i = ncv - 1
    start = nvar*nvar*ncoeff*(i-1) + 8
    data[start+0] = PP_coeffs.aW[i]
    cols[start+0] = nvar*i - 2
    data[start+1] = PU_coeffs.aW[i]
    cols[start+1] = nvar*i - 1
    data[start+2] = PP_coeffs.aP[i]
    cols[start+2] = nvar*i
    data[start+3] = PU_coeffs.aP[i]
    cols[start+3] = nvar*i + 1
    data[start+4] = UP_coeffs.aW[i]
    cols[start+4] = nvar*i - 2
    data[start+5] = UU_coeffs.aW[i]
    cols[start+5] = nvar*i - 1
    data[start+6] = UP_coeffs.aP[i]
    cols[start+6] = nvar*i
    data[start+7] = UU_coeffs.aP[i]
    cols[start+7] = nvar*i + 1

    rows[start:start+4] = nvar*i
    rows[start+4:start+8] = nvar*i + 1

    # Return the matrix
    return csr_matrix((data, (rows, cols)))

def solve(PP_coeffs, PU_coeffs, UP_coeffs, UU_coeffs):
    """Function to solve the linear system and return the correction fields"""

    # Get number of control volumes (check that all are consistent)
    ncv = PP_coeffs.ncv
    if ncv is not PU_coeffs.ncv or ncv is not UP_coeffs.ncv or ncv is not UU_coeffs.ncv:
        raise ValueError("Not all coefficient arrays have the same dimension")

    # Get the sparse matrix
    A = get_sparse_matrix(PP_coeffs, PU_coeffs, UP_coeffs, UU_coeffs)

    # Create and fill the right side vector
    b = np.zeros(2*ncv)
    for i in range(ncv):
        b[2*i] = - PP_coeffs.rP[i] - PU_coeffs.rP[i]
        b[2*i+1] = - UP_coeffs.rP[i] - UU_coeffs.rP[i]

    # Solve the linear system
    res = spsolve(A,b)

    # Extract the correction fields
    dP = np.zeros(ncv)
    dU = np.zeros(ncv)
    for i in range(ncv):
        dP[i] = res[2*i]
        dU[i] = res[2*i+1]

    return dP, dU
