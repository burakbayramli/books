function demoGauss(np,nn)
% demoGauss  Use Gauss-Legendre quadrature to integrate x*exp(-x) on [0,5]
%
% Synopsis:  demoGauss
%            demoGauss(np)
%            demoGauss(np,nn)
%
% Input:   np = (optional) panels used in node refinement test.  Default: np=1
%          nn = (optional) nodes used in panel refinement test.  Default: nn=3
%
% Output:  Tables integral values obtained with Gauss-Legendre quadrature
%          as function of increasing nodes and increasing number of panels

if nargin<1;  np = 1;  end
if nargin<2;  nn = 3;  end
a = 0;  b = 5;   Iexact = -exp(-b)*(1+b) + exp(-a)*(1+a);

% --- Truncation error as function of number of nodes
H = (b-a)/np;
fprintf('\nGauss-Legendre quadrature with %d panels, H = %f\n',np,H);
fprintf('\n nodes       I            error\n');
for n = 1:8
  I = gaussQuad('xemx',a,b,np,n);
  fprintf('%4d  %14.10f %12.2e\n',n,I,I - Iexact)
end

% --- Truncation error as function of panel size
fprintf('\nGauss-Legendre quadrature with %d nodes\n',nn);
fprintf('\n panels    H            I            error       alpha\n');
k = 1;   %  alpha is computed only if k>1
for npanel = 1:8
  I = gaussQuad('xemx',a,b,npanel,nn);
  err = I - Iexact;
  H = (b-a)/npanel;              %  Compute H for output only
  fprintf(' %4d %10.5f %14.10f %12.2e',npanel,H,I,err);

  if k>1,  fprintf('  %8.2f\n',log(err/errold)/log(H/HHold));
  else,    fprintf('\n');  end
  HHold = H;  errold = err;   k = k + 1;  %  prep for next stepsize
end
