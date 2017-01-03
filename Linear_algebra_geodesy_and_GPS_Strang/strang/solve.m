function x = solve(A,b)
%SOLVE	Particular solution to a system of simultaneous linear equations.
%	x = SOLVE(A,b) uses the pivoting LU factorization computed by PLU
%	to find a particular solution of the linear equations A*x = b.
%	The general solution is x + Z*s where Z = NULL(A) and s is an
%	an arbitrary vector.
%
%	The MATLAB backslash operator, x = A\b, and the pseudoinverse function,
%	x = PINV(A)*b, also produce particular solutions to A*x = b.  But if A
%	does not have full rank, the three solutions will usually be different.
%
%	See also SLV, PLU, NULL, \, REF.

[P,L,U,pivcol] = plu(A);
[m,n] = size(A);
if any(size(b) ~= [m,1])
   error('In solve(A,b), b must be a column vector with as many rows as A.')
end

% The rank is the number of pivot columns.
r = length(pivcol);

% Permute the right hand side.
b = P*b;

% Forward elimination to solve L*c = b.
% Note that L is lower triangular with 1's on the diagonal.

c = zeros(m,1);
for k = 1:m
   s = 0;
   for j = 1:k-1
      s = s + L(k,j)*c(j);
   end
   c(k) = b(k) - s;
end

% Check if equations are consistent.  The last m-r entries
% of the new right side c should be negligible.

tol = 1.e-6;
if r < m
   if max(abs(c(r+1:m))) > tol
      error('The equations are not consistent.')
   end
end

% If consistent, use back substitution to find the particular
% solution of U*x = c which uses only the pivot columns.

x = zeros(n,1);
for k = r:-1:1
   t = 0;
   for j = k+1:r
      p = pivcol(j);
      t = t + U(k,p)*x(p);
   end
   p = pivcol(k);
   x(p) = (c(k) - t)/U(k,p);
end
