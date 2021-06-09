function [yhat,aa,bb,cc,dd] = splint(x,y,xhat,opt1,opt2)
% splint  Cubic-spline interpolation with various end conditions
%
% Synopsis:  yhat = splint(x,y,xhat)
%            yhat = splint(x,y,xhat,endType)
%            yhat = splint(x,y,xhat,fp1,fpn)
%            [yhat,a,b,c,d] = splint(x,y,xhat)
%            [yhat,a,b,c,d] = splint(x,y,xhat,endType)
%            [yhat,a,b,c,d] = splint(x,y,xhat,fp1,fpn)
%
% Input:  x,y  = vectors of discrete x and y = f(x) values
%         xhat = (scalar or vector) x value(s) where interpolant is evaluated 
%         endType = (string, optional) either 'natural' or 'notaKnot';  used
%             to select either type of end conditions.  End conditions must be
%             same on both ends. Default: endType='notaKnot'.  For fixed slope
%             end conditions, values of f'(x) are specified, not endType
%         fp1 = (optional) slope at x(1), i,e., fp1 = f'(x(1))
%         fpn = (optional) slope at x(n), i,e., fpn = f'(x(n));
%
% Output:  yhat = (vector or scalar) value(s) of the cubic spline interpolant
%                 evaluated at xhat.  size(yhat) = size(xhat)
%          a,b,c,d = (optional) coefficients of cubic spline interpolants

% --- Process optional input arguments
if nargin<3
  error('minimum of three input arguments needed');
elseif nargin==3
  endType = 'notaknot';
elseif nargin==4   % four input arguments => natural or not-a-knot end conditions
  if ~ischar(opt1)
    error('Third argument must be a string indicating type of end conditions');
  end
  if strncmp('not',lower(opt1),3)
    endType = 'notaknot';
  elseif strncmp('nat',lower(opt1),3)
    endType = 'natural';
  else
    error(sprintf('endType = %s not allowed',endType));
  end
elseif nargin==5  %  five input arguments => fixed slope end conditions
  if ischar(opt1) | ischar(opt2)
    error('Fourth, and fifth arguments must be numbers');
  end
  yp1 = opt1;   ypn = opt2;  endType = 'fixSlope';
else
  error(sprintf('%d input arguments not allowed',nargin));
end

% --- Set up system of equations for b(i)
x = x(:);  y = y(:);  xhat = xhat(:);  %  convert to column vectors
n = length(x);
dx = diff(x);                %  Vector of x(i+1) - x(i) values
divdif = diff(y)./dx;        %  divided difference, f[x(i),x(i+1)]

alpha = [0; dx(1:n-2); 0];                  %  sub diagonal
bbeta = [1; 2*(dx(1:n-2)+dx(2:n-1)); 1];    %  main diagonal
gamma = [0; dx(2:n-1); 0];                  %  super diagonal
A     = tridiags(n,bbeta,alpha,gamma);      %  Sparse, tridiagonal matrix
delta = [ 0; 3*(divdif(2:n-1).*dx(1:n-2) + divdif(1:n-2).*dx(2:n-1)); 0 ];

% --- Modify system of equations as appropriate for the end conditions
if strncmp('not',lower(endType),3)          %  not a knot
  A(1,1) = dx(2);    A(1,2) = dx(1) + dx(2);            % Equation for b(1)
  delta(1) = ( dx(2)*(2*dx(2)+3*dx(1))*divdif(1)...
               + dx(1)*dx(1)*divdif(2) ) /(dx(1)+dx(2));
  A(n,n-1) = dx(n-2) + dx(n-1);    A(n,n) = dx(n-2);    % Equation for b(n)
  delta(n) = ( dx(n-2)*(2*dx(n-2)+3*dx(n-1))*divdif(n-1) ...
               + dx(n-1)*dx(n-1)*divdif(n-2) ) / (dx(n-2)+dx(n-1));
elseif strncmp('nat',lower(endType),3)     %  natural end conditions
  A(1,2) = 0.5;  delta(1) = 1.5*divdif(1);               %  y''(x(1)) = 0
  A(n,n-1) = 1;  A(n,n) = 2;  delta(n) = 3*divdif(n-1);  %  y''(x(n)) = 0
elseif strncmp('fix',lower(endType),3)     %  prescribed slope end conditions
  delta(1) = yp1;  delta(n) = ypn;
else
  error(sprintf('Logic error:  endType = %s',endType));
end

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

if nargout>1,  aa = a;  bb = b;  cc = c;  dd = d;  end  %  optional outputs
