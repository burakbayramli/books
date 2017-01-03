function C = Cholesky(A)
% Cholesky  Cholesky factorization of a symmetric, positive definite matrix
%
% Synopsis:  C = Cholesky(A)
%
% Input:     A = symmetric positive definite matrix
%
% Output:    C = upper triangular matrix such that A = C'*C

[m,n] = size(A);
if m~=n,  error('A must be square');  end
C = zeros(n,n);

for i=1:n
  for j=i:n
    if j==1
      s = A(i,i);   %  i=1, j=1 is special case
    else
      s = A(i,j) - C(1:i-1,i)'*C(1:i-1,j);
    end
    if j>i
      C(i,j) = s/C(i,i);
    else
      if s<=0, error('C is not positive definite to working precision'); end
      C(i,i) = sqrt(s);
    end
  end
end
