function yhat = hermint(x,f,fp,xhat)
% hermint  Piecewise-cubic Hermite interpolation
%
% Synopsis: yhat = hermint(x,f,fp,xhat)
%
% Input:    x     = vector of independent variable values
%           f, fp = vectors of f(x) and f'(x)
%           xhat = (scalar or vector) x values where interpolant is evaluated
%
% Output:   yhat = scalar or vector value of cubic hermite interpolant at
%                  x = xhat.  size(yhat) = size(xhat)

n = length(x);
if     length(f)~=n,    error('x and f are not compatible');
elseif length(fp)~=n,   error('x and fp are not compatible');   end

% --- Construct coefficients of the piecewise interpolants
x = x(:);  xhat = xhat(:);    %  Convert to column vectors
f = f(:);  fp = fp(:);
dx = diff(x);                 %  Vector of x(i+1) - x(i) values
divdif = diff(f)./dx;         %  Vector of divided differences, f[x(i),x(i+1)]
a = f(1:n-1);
b = fp(1:n-1);
c = ( 3*divdif - 2*fp(1:n-1) - fp(2:n) ) ./dx;
d = ( fp(1:n-1) - 2*divdif + fp(2:n) )   ./dx.^2;

% --- Locate each xhat value in the x vector
i = zeros(size(xhat));   %  i is index into x such that x(i) <= xhat <= x(i+1)
for m=1:length(xhat)     %  For vector xhat: x( i(m) ) <= xhat(m) <= x( i(m)+1 )
  i(m) = binSearch(x,xhat(m));
end

% --- Nested, vectorized evaluation of the piecewise polynomials
xx = xhat - x(i);
yhat = a(i) + xx.*(b(i) + xx.*(c(i) + xx.*d(i)) );
