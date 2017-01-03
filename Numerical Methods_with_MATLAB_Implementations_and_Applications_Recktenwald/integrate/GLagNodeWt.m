function [x,w] = GLagNodeWt(n)
% GLagNodeWt  Nodes and weights for Gauss-Laguerre quadrature of arbitrary order
%             by solving an eigenvalue problem
%
% Synopsis:  [x,w] = GLagNodeWt(n)
%
% Input:     n = order of quadrature rule
%
% Output:    x = vector of nodes
%            w = vector of weights

%  Algorithm based on ideas from Golub and Welsch, and Gautschi.  For a
%  condensed presentation see H.R. Schwarz, "Numerical Analysis: A
%  Comprehensive Introduction", 1989, Wiley.  Original MATLAB
%  implementation by H.W. Wilson and L.H. Turcotte, "Advanced Mathematics
%  and Mechanics Applications Using MATLAB", 2nd ed., 1998, CRC Press

J = diag(1:2:2*n-1) - diag(1:n-1,1) - diag([1:n-1],-1);  % Jacobi matrix
[V,D]  = eig(J);
[x,ix] = sort(diag(D));  %  nodes are eigenvalues, which are on diagonal of D
w      = 1*V(1,ix)'.^2;  %  V(1,ix)' is column vector of first row of sorted V 
