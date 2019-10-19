function [L,U] = mylu(A)
% syntax: [L,U] = mylu(A)
% input: matrix A
% perform an LU factorization of A

[m,n] = size(A);

L = zeros(n,n); U = zeros(n,n);

for i=1:n
  L(i:n,i) = A(i:n,i);  

  if i > 1
    J = 1:i-1;
    L(i:n,i) = L(i:n,i) - L(i:n,J)*U(J,i);
    U(i,i:n) = [1.0 (A(i,i+1:n)-L(i,J)*U(J,i+1:n))/L(i,i)];
  else
    U(i,i:n) = [1.0 A(i,i+1:n)/L(i,i)];
  end;
  
end;

return;
