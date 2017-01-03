function [L,U] = luNopivVec(A,ptol)
% luNopivVec  LU factorization without pivoting - vectorized implementation
%             Row operations to compute submatrix A(i+1:n,i+1:n) resulting
%             from elimination of partial column A(i:1:n,i) are performed
%             as block matrix operation
%
% Synopsis:  [L,U] = luNopivVec(A)
%            [L,U] = luNopivVec(A,ptol)
%
% Input:     A    = coefficient matrix
%            ptol = (optional) tolerance for detection of zero pivot
%                   Default:  ptol = 50*eps
%
% Output:    L,U = lower triangular matrix, L, and upper triangular
%                  matrix, U, such that A = L*U

if nargin<2, ptol = 50*eps;  end
[m,n] = size(A);
if m~=n,  error('A matrix needs to be square');  end

for i = 1:n-1                  %  loop over pivot rows
  pivot = A(i,i);
  if abs(pivot)<ptol, error('zero pivot encountered');  end
  A(i+1:n,i) = A(i+1:n,i)/pivot;            %  partial column of multipliers
  A(i+1:n,i+1:n) = A(i+1:n,i+1:n) - ...     %  row ops to eliminate A(i+1:n,i)
                   A(i+1:n,i)*A(i,i+1:n);   %  implemented as outer product update
end

L = eye(size(A)) + tril(A,-1);  %  extract L and U
U = triu(A);
