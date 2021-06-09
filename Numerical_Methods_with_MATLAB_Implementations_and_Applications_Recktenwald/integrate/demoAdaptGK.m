function demoAdaptGK
% demoAdaptGK  Compare adaptive Gauss-Kronrod quadrature with quadl
%
% Synopsis:  demoAdaptGK
%
% Input:     none
%
% Output:  Tables and plots demonstrating convergence of the adaptive G-K
%          method as error tolerance decreases.  Comparison to quadl

t = [5e-2;  5e-3;  5e-5;  5e-9];
tol = [t t];  %  two columns: relative and absolute error tolerance

% --- Evaluate integral form of the bessel function
fun = inline('cos(z*sin(x) - nu*x)','x','nu','z');
nu = 2;  z = 8;
Je = besselj(nu,z);
for i = 1:length(tol)
  flops(0);  JGK(i) = adaptGK(fun,0,pi,tol(i,:),[],nu,z)/pi;   fGK(i) = flops;
  flops(0);  JL(i)  = quadl(fun,0,pi,tol(i,:),[],nu,z)/pi;      fL(i) = flops;
end
eL = JL - Je;   eGK = JGK - Je;   %  errors in numerical values of integral

fprintf('\nComputed J%d(%f):\n',nu,z)
fprintf('\n    rel tol         eGK          fGK       equadl      fquadl\n');
for i = 1:length(tol)
  fprintf('%12.2e  %12.2e  %8d  %12.2e  %8d\n',tol(i,1),eGK(i),fGK(i),eL(i),fL(i));
end

eGK(eGK==0) = eps;  % avoid error in loglog plot
ferr = figure;
subplot(2,1,1);
loglog(abs(eGK),tol,'o--',abs(eL),tol,'^-');
legend('adaptGK','quadl');  xlabel('Absolute error');   ylabel('tol');
title(sprintf('Compare GK and quadl for besselj(%d,%d)',nu,z));
subplot(2,1,2);
loglog(abs(eGK),fGK,'o--',abs(eL),fL,'^-');
legend('adaptGK','quadl');  xlabel('Absolute error');   ylabel('flops');

% --- Integrate humps
a = 0;  b = 2;  Ie = humpInt(a,b);
fprintf('\nIntegral of humps for %f to %f:\n',a,b)
for i = 1:length(tol)
  flops(0);  IGK(i) = adaptGK('humps',a,b,tol(i,:));   fGK(i) = flops;
  flops(0);  IL(i) = quadl ('humps',a,b,tol(i,:));      fL(i) = flops;
end
eL = IL - Ie;   eGK = IGK - Ie;   %  errors in numerical values of integral

fprintf('\n    rel tol         eGK          fGK       equadl      fquadl\n');
for i = 1:length(tol)
  fprintf('%12.2e  %12.2e  %8d  %12.2e  %8d\n',tol(i,1),eGK(i),fGK(i),eL(i),fL(i));
end

eGK(eGK==0) = eps;   eL(eL==0) = eps;   % avoid error on log-log plot
ferr2 = figure;
subplot(2,1,1);
loglog(abs(eGK),tol,'o--',abs(eL),tol,'^-');
legend('adaptGK','quadl');  xlabel('Absolute error');   ylabel('tol');
title('Compare GK and quadl for humps');
subplot(2,1,2);
loglog(abs(eGK),fGK,'o--',abs(eL),fL,'^-');
legend('adaptGK','quadl');  xlabel('Absolute error');   ylabel('flops');


