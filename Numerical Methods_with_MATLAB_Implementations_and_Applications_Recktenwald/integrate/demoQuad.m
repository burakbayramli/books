function demoQuad(a,b)
% demoQuad  Use built in quad and quad8 to integrate 'humps' on [0,2]
%
% Synopsis:  demoQuad
%            demoQuad(a,b)
%
% Input:  a,b = (optional) upper and lower limits of integral
%               Default:  a = 0;  b = 2
%
% Output: Tables of absolute error and flops as function of tol parameter
%         input to quad and quad8.  Plots of error vs. tol and error vs. flops

if nargin<2,  a=0; b=2;  end

tol = [5e-2  5e-3  5e-4  5e-5];    %  sequence of relative tolerances
for k = 1:length(tol)
  flops(0);  q(k)  = quad('humps',0,2,tol(k));   f(k) = flops;
  flops(0);  q8(k) = quad8('humps',0,2,tol(k));  f8(k) = flops;
end
Iexact = humpInt(0,2);  e = abs(q - Iexact);  e8 = abs(q8 - Iexact);

fprintf('                -----  quad -----     ----- quad8 -----\n');
fprintf('     tol         error      flops      error       flops\n');
for k = 1:length(tol)
  fprintf('%11.2e  %11.2e  %6d   %11.2e   %6d\n',tol(k),e(k),f(k),e8(k),f8(k));
end

subplot(2,1,1);
loglog(e,tol,'o--',e8,tol,'^-');
legend('quad','quad8',2);  xlabel('Absolute error');  ylabel('tol');

subplot(2,1,2);
loglog(e,f,'o--',e8,f8,'^-');
legend('quad','quad8');   xlabel('Absolute error');   ylabel('flops');
