function A = tridiags(n,a,b,c)
% tridiags  Create sparse tridiagonal matrix from two or three scalars or vectors
%
% Synopsis:  A = tridiags(n,a,b)
%            A = tridiags(n,a,b,c)
%
% Input:    n     = size of desired (n by n) matrix
%           a,b,c = scalar or vector values for the main, super and sub diagonals.
%                   If a, b, or c are scalars, they are replicated to fill the
%                   diagonals.  If c is not supplied, c = b is assumed
%
% Output:   A = sparse, tridiagonal matrix of the form
%
%                a  b  0  0               a(1)  b(1)   0     0
%                c  a  b  0       or      c(2)  a(2)  b(2)   0
%                0  c  a  b                0    c(3)  a(3)  b(3)
%                0  0  c  a                0     0    c(4)  a(4)

if nargin<4,  c = b;  end

if length(a)==1,  a = a*ones(n,1);  end    %  replicate scalars
if length(b)==1,  b = b*ones(n,1);  end
if length(c)==1,  c = c*ones(n,1);  end
if length(a)<n | length(b)<n | length(c)<n
  error(sprintf('a,b,c must be scalars or have length >= %d',n));
end

a = a(:);  b = b(:);  c = c(:);   %  guarantee column vectors
A = spdiags([ [c(2:n); 0] a [0; b(1:n-1)] ],[-1 0 1], n,n);
