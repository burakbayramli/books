function x = slv(A,b)
%SLV	Simple linear equation solver.
%	x = SLV(A,b) tries to use the LU factorization computed by SLU(A)
%	to solve the linear equation A*x = b.
%	Since SLU does no pivoting, SLV may fail if a small pivot is
%	encountered.
%
%	See also SLU, SOLVE, \ .

[L,U] = slu(A);

% Forward elimination to solve L*c = b.
% Note that L is lower triangular with 1's on the diagonal.

[n,n] = size(A);
for k = 1:n
   s = 0;
   for j = 1:k-1
      s = s + L(k,j)*c(j);
   end
   c(k) = b(k) - s;
end

% Back substitution to solve U*x = c.
% Note that U is upper triangular with nonzeros on the diagonal.

for k = n:-1:1
   t = 0;
   for j = k+1:n
      t = t + U(k,j)*x(j);
   end
   x(k) = (c(k) - t)/U(k,k);
end
x = x';
