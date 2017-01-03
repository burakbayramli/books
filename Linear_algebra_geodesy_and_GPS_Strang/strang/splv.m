function x = splv(A,b)
%SPLV	The solution to a square, invertible system.
%	x = SPLV(A,b) uses the PA = LU factorization
%	computed by SPLU to solve A*x = b.
%
%	The MATLAB backslash operator A\b also finds x.
%
%	See also SLV, SPLU.

[P,L,U] = splu(A);
[n,n] = size(A);

% Permute the right hand side.
b = P*b;

% Forward elimination to solve L*c = b.

c = zeros(n,1);
for k = 1:n
   s = 0;
   for j = 1:k-1
      s = s + L(k,j)*c(j);
   end
   c(k) = b(k) - s;
end

% Back substitution to solve U*x = c.

x = zeros(n,1);
for k = n:-1:1
   t = 0;
   for j = k+1:n
      t = t + U(k,j)*x(j);
   end
   x(k) = (c(k) - t)/U(k,k);
end
