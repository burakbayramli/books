function [yhat,dy,cout] = newtint(x,y,xhat)
% newtint  Interpolation with Newton polynomials of arbitrary degree
%
% Synopsis:  yhat       = newtint(x,y,xhat)
%           [yhat,dy]   = newtint(x,y,xhat)
%           [yhat,dy,c] = newtint(x,y,xhat)
%
% Input:     x,y = vectors containing the tabulated y = f(x) data
%            xhat = (scalar or vector) x values where interpolant is evaluated 
%
% Output:    yhat = value of y at each xhat obtained via interpolation
%            dy   = (optional) change in value of interpolant between
%                   polynomials of degree m-1 and m.  m = n-1 = length(x)-1
%            c    = (optional) coefficients of the Newton form of polynomial
%
% Note:      Degree of interpolating polynomial is implicitly specified
%            by the length of the x and y vectors.  If n = length(y) then
%            yhat is evaluated with a polynomial of degree (n-1)

n = length(y);   if length(x)~=n,  error('x and y are not compatible');  end

% --- Construct polynomial coefficients from diagonal of div.-diff. table
c = y(:);        %  First column is zeroth-order difference, f[x_i] = y_i
for j=2:n
  for i=n:-1:j   %  Work backward to keep from overwriting unused data
    c(i) = (c(i)-c(i-1))/(x(i)-x(i-j+1));
  end
end

% --- Nested evaluation of the polynomial
yhat = c(n);
for i=n-1:-1:1
  yhat = yhat.*(xhat-x(i)) + c(i);   %  Array op allows vector of xhats
end

% --- optional output
if nargout>1
  yn2 = c(n-1);     %  begin evaluation of polynomial of degree n-2
  for i=n-2:-1:1
    yn2 = yn2.*(xhat-x(i)) + c(i);
  end
  dy = yhat - yn2;  %  difference of interpolants of degree n-2 and degree n-1
  if nargout>2, cout = c;  end   %  copy coefficients to output variable
end
