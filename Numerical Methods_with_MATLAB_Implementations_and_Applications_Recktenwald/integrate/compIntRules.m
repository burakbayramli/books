function compIntRules
% compIntRules  Compare trapezoid, simpson, and Gauss-Legendre quadrature rules
%
% Synopsis:  compIntRules
%
% Input:  none
%
% Output:  Table and plot of truncation errors for each integration method
%          as a function of the total number of function evaluations

xmin = 0;  xmax = 5;
fun = inline('x.*exp(-x)');            %  integrand, f(x)
ifun = inline('-exp(-x).*(1+x)');      %  integral of f(x)
Iexact = ifun(xmax) - ifun(xmin);

% --- Vectors defining the number of panels used for each method.
%     The number of nodes per panel is different for each one
npt = [ 1  3  7  15  31  63  127  255];    %  panels for Trapezoid rule
nps = [ 1  2  4   8  16  32   64  128];    %   "     "   Simpson's rule
npg = [ 1  1  2   4   8  16   32   64];    %   "     "   Gauss-Legendre rule
nng = [ 2  4  4   4   4   4    4    4];    %  nodes in each panel of G-L rule

nft = npt + 1;    %  number of function evaluations with Trapezoid rule
nfs = 2*nps + 1;  %  ... with Simpson's rule
nfg = npg.*nng;   %  ... with Gauss-Legendre rule

% --- Compute truncation errors
for k=1:length(npt);
  errt(k) = trapezoid(fun,xmin,xmax,npt(k)) - Iexact;
  errs(k) = simpson(fun,xmin,xmax,npt(k)) - Iexact;
  errg(k) = gaussquad(fun,xmin,xmax,npg(k),nng(k)) - Iexact;
end

% --- plot and print results
loglog(nft,abs(errt),'o',nfs,abs(errs),'+',nfg,abs(errg),'s');
GLstr = sprintf('GL %d node',fix(max(nng)));  %  string used in legend
legend('trapezoid','simpson',GLstr);
ylabel('Truncation error');
xlabel('Number of function evaluations');

fprintf('\nTruncation error versus number of function evaluations\n\n');
fprintf('     Trapezoid             Simpson              %s\n',GLstr);
fprintf('    nf     error         nf      error        nf      error\n');
for k=1:length(npt)
  fprintf('%6d  %11.2e  %6d  %11.2e  %6d  %11.2e\n',...
           nft(k),errt(k),nfs(k),errs(k),nfg(k),errg(k));
end
