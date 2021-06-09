function A = tridiag(n,a,b,c)
% tridiag  Create tridiagonal matrix from two or three scalars or vectors.
%
% Synopsis:  A = tridiag(n,a,b)
%            A = tridiag(n,a,b,c)
%
% Input:    n = size of desired (n by n) matrix
%           a,b,c = scalar or vector values for the main, super and sub
%                   diagonals.  If a, b, or c are scalars, they are
%                   replicated to fill the diagonals.  If a value of c
%                   is not supplied, c = b is assumed
%
%  Output:   A = tridiagonal matrix of the form
%
%                a  b  0  0         a(1)  b(1)   0     0
%                c  a  b  0         c(2)  a(2)  b(2)   0
%                0  c  a  b    or    0    c(3)  a(3)  b(3)
%                0  0  c  a          0     0    c(4)  a(4)

if nargin<4,  c = b;  end

if length(a)==1,  a = a*ones(n,1);  end    %  replicate scalars
if length(b)==1,  b = b*ones(n,1);  end
if length(c)==1,  c = c*ones(n,1);  end
if length(a)<n | length(b)<n | length(c)<n
  error(sprintf('a,b,c must be scalars or have length >= %d',n));
end

a = a(:);  b = b(:);  c = c(:);   %  guarantee column vectors
A = diag(a) + diag(b(1:n-1),1) + diag(c(2:n),-1);
