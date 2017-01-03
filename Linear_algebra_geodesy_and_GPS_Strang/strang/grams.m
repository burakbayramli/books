function [Q,R] = grams(A)
%GRAMS	Gram-Schmidt orthogonalization.
%	[Q,R] = grams(A) returns matrix Q with orthonormal
%	columns and an upper triangular R so that Q*R = A.
%	Warning: this algorithm is numerically unstable.
%	Use MATLAB's built-in QR function for stability.

[m,n] = size(A);
Asave = A;
for j = 1:n
   for k = 1:j-1
      A(:,j) = A(:,j) - (A(:,j)'*A(:,k))/(A(:,k)'*A(:,k))*A(:,k);
   end
end
tol = 1.e-6;
for j = 1:n
   if norm(A(:,j)) < tol
      error('Columns are linearly dependent.')
   end
   Q(:,j) = A(:,j)/norm(A(:,j));
end
R = Q'*Asave;
