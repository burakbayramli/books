#!/usr/bin/env python

def buildsparse(connectivity):
    """
    Build a sparse matrix as a dictionary:
    smat[(i,j)] holds matrix with row-column pair (i,j).
    smat is returned.
    
    Input is the connectivity array associated with finite
    element grids. connectivity(e,r) gives the global matrix
    degree of freedom (i or j) corresponding to a local degree
    of freedom r in an element e.
    """
    smat = {}
    nel = connectivity.shape[0]  # no of elements
    nne = connectivity.shape[1]  # no of nodes in an element
    for e in range(nel):
        for r in range(nne):
            for s in range(r+1):
                i = connectivity[e, r]
                j = connectivity[e, s]
                smat[(i,j)] = 0.0   # add (i,j) pair
                smat[(j,i)] = 0.0   # matrix pattern is symmetric
    return smat

def visualize(s, n):
    """
    Visualize sparsity pattern of a matrix; print a * where a
    nonzero entry occurs.
    """
    t = []
    for i in range(n):
        t.append([])
        for j in range(n):
            t[i].append(' ')
    for i in range(n):
        for j in range(n):
            if (i,j) in s:
                t[i][j] = '*'
    for i in range(n):
        for j in range(n):
            print t[i][j],
        print

if __name__ == '__main__':
    from numpy import zeros
    n = 6  # no of nodes in one space direction
    nel = (n-1)*(n-1)
    nne = 4 # bilinear 2D elements
    c = zeros((nel,nne), int)
    for j in range(n-1):
        for i in range(n-1):
            e = j*(n-1) + i
            c[e,0] = j*n + i
            c[e,1] = j*n + i+1
            c[e,2] = (j+1)*n + i
            c[e,3] = (j+1)*n + i+1
    for e in range(nel):
        print 'element %d, nodes' % e,
        for i in range(nne):
            print c[e,i],
        print
    matrix = buildsparse(c)
    indices = matrix.keys()
    indices.sort()
    for pair in indices:
        print pair
    visualize(matrix, n*n)
    

            
