function [p,e,t]  = bsp01g
% Kreisscheibe mit PDE TOOLBOX
[p,e,t] = initmesh('circleg','Hmax',0.2);
p       = jigglemesh(p,e,t);
AUX      = mesh65(t,e);
AUX      = AUX(3,:)';
%[p,e,t]  = refinemesh('circleg',p,e,t,AUX);

