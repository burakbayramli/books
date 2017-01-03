% Strang Linear Algebra Toolbox
% Version 1.0, 10-September-93.
% Copyright (c) 1993 by Gilbert Strang.
%
% For use with
%   Introduction to Linear Algebra   by  Gilbert Strang  (1993)
%   Wellesley-Cambridge Press   Box 812060   Wellesley MA 02181
%
% Teaching Codes
%   basis      - Basis for the column space.
%   cofactor   - Cofactors and the cofactor matrix.
%   cramer     - Solve a linear system by Cramer's Rule.
%   determ     - Matrix determinant from the pivots in PA = LU.
%   eigen      - Describe the eigenvalues and eigenvectors.
%   eigen2     - Two by two eigenvalues and eigenvectors.
%   findpiv    - Used by PLU to find pivots for elimination.
%   grams      - Orthonormal vectors from Gram-Schmidt.
%   inverse    - Matrix inverse by Gauss-Jordan elimination.
%   linefit    - Plot the least squares fit by a line.
%   lsq        - Compute the least squares solution to Ax = b.
%   null       - Nullspace matrix: n - r solutions to Ax = 0
%   permdet    - Determinant of a permutation.
%   plot2d     - Show linear transformations of the plane.
%   plu        - PA = LU for any A, with row exchanges in P.
%   projmat    - Projection matrix onto the column space.
%   randperm   - Random permutation.
%   rats       - Convert to nearby rational numbers.
%   ref        - Reduced echelon form R from elimination on [A I]
%   signperm   - Sign of a permutation: 1 or -1 for even or odd.
%   slu        - Square LU factorization but no row exchanges.
%   slv        - Solve square Ax = b using L and U from SLU.
%   solve      - Particular solution to consistent Ax = b.
%   splu       - PA = LU factorization of square invertible A.
%   splv       - Unique solution to Ax = b using SPLU.
%
% Each teaching code has a companion explanatory "x" code.
%
% Extra codes, examples and exercises.
%   addvec     - Two-dimensional plot of vector addition
%   atimesv    - Number times vector in 2D: elementary graphics
%   cosine     - Graphical display of u and v and the cosine formula
%   expage35   - MATLAB tutorial for Markov exercise 30, page 35
%   expage36   - Interactive plot for Markov exercise 32, page 36
%   expower    - Successes and failures of the power method
%   hand       - Coordinates that give the outline of a hand
%   house      - House matrix from 7.1 in the book: Transformations by A
%   fastfour   - Fourier matrix times vector: by FFT if n = 2^k
%   fourier    - The Fourier matrix diagonalizes every circulant matrix
%   linprog    - Minimize cost in linear programming
%   movies     - Note on ratmovie and rrefmovie for reduced echelon form R
%   poly2str   - Convert a polynomial coefficient vector to a string
%   power      - Power method to find the largest eigenvalue (2 by 2)
%   sixpack    - Six ways to multiply AB: Permute "for i" "for j" "for k"
%   zeroone    - Probability that a 0-1 matrix is singular: Experiment
