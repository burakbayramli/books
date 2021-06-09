function [L,U,pv] = luPiv(A,ptol)
% luPiv  LU factorization with partial pivoting
%
% Synopsis:  [L,U,pv] = luPiv(A)
%            [L,U,pv] = luPiv(A,ptol)
%
% Input:     A    = coefficient matrix
%            ptol = (optional) tolerance for detection of zero pivot
%                   Default:  ptol = 50*eps
%
% Output:    L,U = lower triangular matrix, L, and upper triangular
%                  matrix, U, such that A(pv,:) = L*U
%            pv  = index vector that records row exchanges used to select
%                  good pivots.  The row permutations performed during
%                  elimination can be applied to the right hand side vector
%                  with b(pv).  The L and U returned by luPiv are the
%                  factors of permuted matrix A(pv,:), which is equivalent
%                  to P*A where P is the permutation matrix created
%                  by the two statements P = eye(size(A));  P = P(pv,:).

if nargin<3, ptol = 50*eps;  end     %  Default tolerance for zero pivot
[m,n] = size(A);
if m~=n,  error('A matrix needs to be square');  end
pv = (1:n)';

for i = 1:n-1                        %  loop over pivot row
  [pivot,p] = max(abs(A(i:n,i)));    %  value and index of largest pivot
  ip = p + i - 1;                    %  p is index in subvector i:n
  if ip~=i                           %  ip is true row index of desired pivot
     A([i ip],:) = A([ip i],:);      %  swap the rows
     pv([i ip]) = pv([ip i]);        %  record pivot order
  end
  pivot = A(i,i);
  if abs(pivot)<ptol, error('zero pivot encountered after row exchange');  end
  for k = i+1:n                                   % row k is eliminated next
    A(k,i) = A(k,i)/pivot;                        % compute and store multiplier
    A(k,i+1:n) = A(k,i+1:n) - A(k,i)*A(i,i+1:n);  % row ops to eliminate A(k,i)
  end
end

L = eye(size(A)) + tril(A,-1);  %  extract L and U
U = triu(A);
