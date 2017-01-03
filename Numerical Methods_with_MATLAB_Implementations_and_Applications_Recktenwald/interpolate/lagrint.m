function yi = lagrint(x,y,xi)
% lagrint  Interpolation with Lagrange polynomials of arbitrary degree
%
% Synopsis:  yi = lagrint(x,y,xi)
%
% Input:   x,y = tabulated data
%          xi  = point where interpolation is to be evaluated
%
% Output:    yi = value of y at x = xi obtained via interpolation with
%                 polynomial of degree n-1, where length(y) = length(x) = n

dxi = xi - x;        %  vector of xi - x(1), xi - x(2), ... values
n = length(x);       %  degree of polynomial is n-1
L = zeros(size(y));  %  preallocate L for speed

%  Refer to section 10.2.2 in text for explanation of vectorized code
%  used to compute Lagrange basis functions, L(j)
L(1) = prod(dxi(2:n))/prod(x(1)-x(2:n));       %  j = 1
L(n) = prod(dxi(1:n-1))/prod(x(n)-x(1:n-1));   %  j = n
for j=2:n-1
  num = prod(dxi(1:j-1))*prod(dxi(j+1:n));
  den = prod(x(j)-x(1:j-1))*prod(x(j)-x(j+1:n));
  L(j) = num/den;
end
yi = sum(y.*L);      %  Evaluate Polynomial: sum of y(j)*L(j), j=1..n
