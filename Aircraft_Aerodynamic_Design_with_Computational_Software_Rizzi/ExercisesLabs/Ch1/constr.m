function [cn cl] = constr(x)
global WoSor2 DEMO Itno
if DEMO==1
  prop = cfd(x);
  cn   = -x(2)^2*prop(1) + WoSor2-x(2)^2*prop(2)*tan(x(1));
  cl   = [];
elseif DEMO==2
  a = x(1); V = x(2); th = x(3); T = x(4);
  prop  = cfd(x);
  cl(1) = T*cos(a) - V^2*prop(2) - WoSor2*sin(th);
  cl(2) = T*sin(a) + V^2*prop(1) - WoSor2*cos(th); 
  cn    = [];
else
  error(['DEMO ' num2str(DEMO) ' not impl'])
end