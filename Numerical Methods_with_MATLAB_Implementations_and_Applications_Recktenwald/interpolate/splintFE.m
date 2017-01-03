function yhat = splintFE(x,y,xhat,fp1,fpn)
% splintFE  Cubic-spline interpolation with fixed slope end conditions
%
% Synopsis:   yhat = splintFE(x,y,xhat,fp1,fpn)
%
% Input:   x,y  = vectors of discrete x and y = f(x) values
%          xhat = (scalar or vector) x values where interpolant is evaluated 
%          fp1  = slope at x(1), i,e., fp1 = f'(x(1))
%          fpn  = slope at x(n), i,e., fpn = f'(x(n));
%
% Output:  yhat = (vector or scalar) value(s) of the cubic spline interpolant
%                 evaluated at xhat.  size(yhat) = size(xhat)

% --- Set up system of equations for b(i)
x = x(:);  y = y(:);  xhat = xhat(:);  %  convert to column vectors
n = length(x);
dx = diff(x);                 %  vector of x(i+1) - x(i) values
divdif = diff(y)./dx;         %  vector of divided differences, f[x(i),x(i+1)]

alpha = [0; dx(1:n-2); 0];                  %  sub diagonal
bbeta = [1; 2*(dx(1:n-2)+dx(2:n-1)); 1];    %  main diagonal
gamma = [0; dx(2:n-1); 0];                  %  super diagonal
A     = tridiags(n,bbeta,alpha,gamma);      %  Sparse, tridiagonal matrix
delta = [ fp1; ...
          3*(divdif(2:n-1).*dx(1:n-2) + divdif(1:n-2).*dx(2:n-1)); ...
          fpn ];

% --- Solve the system for b
mmdflag = spparms('autommd');     %  Store minimum degree ordering flag
spparms('autommd',0);             %  Set that flag to zero
b = A\delta;                      %  Solve the system
spparms('autommd',mmdflag);       %  Reset the minimum degree ordering flag

% --- Compute coefficients of spline interpolants
a = y(1:n-1);
c = (3*divdif - 2*b(1:n-1) - b(2:n))./dx;
d = (b(1:n-1) - 2*divdif + b(2:n))./dx.^2;
b(n) = [];                                  %  discard b(n)

% --- Locate each xhat value in the x vector
i = zeros(size(xhat));  %  i is index into x such that x(i) <= xhat <= x(i+1)
for m=1:length(xhat)    %  For vector xhat: x( i(m) ) <= xhat(m) <= x( i(m)+1 )
  i(m) = binSearch(x,xhat(m));
end

% --- Nested, vectorized evaluation of the piecewise polynomials
xx = xhat - x(i);
yhat = a(i) + xx.*(b(i) + xx.*(c(i) + xx.*d(i)) );
