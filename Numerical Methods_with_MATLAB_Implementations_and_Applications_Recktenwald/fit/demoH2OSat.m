function demoH2Osat
% demoH2OSat  Fit saturation pressure versus temperature for water
%
% Synopsis:  demoH2Osat
%
% Input:   none
%
% Output:  Print curve fit coefficients.  Plot fit fcn vs. original data

% --- load data, convert T to Kelvin, p to log(p)
[t,p] = loadColData('H2Osat.dat',2,2);   tk = t + 273.15;   lnp = log(p);

% --- inline fcns return A matrix;  columns of A are basis fcns evaluated at x
Afun1 = inline('[1./x  ones(size(x))  x  log(x)]');
Afun2 = inline('[1./x  ones(size(x))  x  x.^2  log(x)]');
Afun3 = inline('[1./x  ones(size(x))  x  x.^2  x.^3  log(x)]');

% --- repeat fit for each set of basis functions
fprintf('\nBasis function set 1:\n');   doFit(tk,p,Afun1);
fprintf('\nBasis function set 2:\n');   doFit(tk,p,Afun2);
fprintf('\nBasis function set 3:\n');   doFit(tk,p,Afun3);

% ======================================
function doFit(tk,p,Afun)
% doFit  Performs steps repeated for each set of basis functions

[c,R2,r] = fitqr(tk,log(p),Afun);
fprintf('c = \n');   fprintf(' %12.4e',c);   fprintf('\n');
fprintf('R2 = %10.6f    ||r||_2 = %10.6f\n',R2,norm(r));

tfit = linspace(min(tk),max(tk))';    %  range of t for eval of fit function
A = Afun(tfit);                       %  columns are basis fcns eval at tfit
lnpfit = A*c;                         %  value of log(p) at tfit
pfit = exp(lnpfit);

f = figure;                     %  new figure window
subplot(2,1,1);       semilogy(tk,p,'o',tfit,pfit,'-');
ylabel('p   (bar)');  legend('data','fit function',2);
subplot(2,1,2);       plot(tk,r,'o'); 
xlabel('T   {}^\circ c');       ylabel('p residual   (bar)');
axis([250  650  -8e-3  8e-3]);  %  allow visual comparison of residuals

