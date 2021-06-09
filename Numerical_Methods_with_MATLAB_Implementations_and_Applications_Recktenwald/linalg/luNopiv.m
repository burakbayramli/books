function [L,U] = luNopiv(A,ptol)
% luNopiv  LU factorization without pivoting
%
% Synopsis:  [L,U] = luNopiv(A)
%            [L,U] = luNopiv(A,ptol)
%
% Input:     A    = coefficient matrix
%            ptol = (optional) tolerance for detection of zero pivot
%                   Default:  ptol = 50*eps
%
% Output:    L,U = lower triangular matrix, L, and upper triangular
%                  matrix, U, such that A = L*U

if nargin<3, ptol = 50*eps;  end     %  Default tolerance for zero pivot
[m,n] = size(A);
if m~=n,  error('A matrix needs to be square');  end

for i = 1:n-1                        %  loop over pivot rows
  pivot = A(i,i);
  if abs(pivot)<ptol, error('zero pivot encountered');  end
  for k = i+1:n                                   %  row k is eliminated next
     A(k,i) = A(k,i)/pivot;                       %  compute and store multiplier
     A(k,i+1:n) = A(k,i+1:n) - A(k,i)*A(i,i+1:n); %  row ops to eliminate A(k,i)
  end
end

L = eye(size(A)) + tril(A,-1);  %  extract L and U
U = triu(A);
