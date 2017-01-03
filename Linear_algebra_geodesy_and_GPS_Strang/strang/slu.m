function [L,U] = slu(A)
%SLU	Simple, square, LU factorization.
%	[L,U] = SLU(A) for a square matrix A, illustrates the use of
%	Gaussian elimination to compute a lower triangular matrix L 
%	and an upper triangular matrix U so that L*U = A.
%
%	The algorithm does no pivoting and so will fail if a small
%	pivot is encountered.
%
%	See also SLV, PLU, LU.

[n,n] = size(A);
tol = 1.e-6;

for k = 1:n
   if abs(A(k,k)) < tol
      disp(['Small pivot encountered in column ' int2str(k)])
   end
   L(k,k) = 1;
   for i = k+1:n
      L(i,k) = A(i,k)/A(k,k);
      for j = k+1:n
         A(i,j) = A(i,j) - L(i,k)*A(k,j);
      end
   end
   for j = k:n
      U(k,j) = A(k,j);
   end
end
