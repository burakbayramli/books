function [dq] = EulerLF2Dx(ql,qr,gamma)
% function [dq] = EulerLF2Dx(ql,qr,gamma)
% Purpose: Evaluate Lax Friedrich numerical flux for Euler's equation along x

% Extract states
rl = ql(:,1); rul = ql(:,2); rvl = ql(:,3); El = ql(:,4);
rr = qr(:,1); rur = qr(:,2); rvr = qr(:,3); Er = qr(:,4);

% Compute fluxes for left and right states
ul = rul./rl; vl = rvl./rl;
pl = (gamma-1)*(El - 0.5*rl.*(ul.^2+vl.^2)); cl = sqrt(gamma*pl./rl);
F1l = rul; F2l = rul.*ul + pl; F3l = rul.*vl; F4l = (El + pl).*ul;

ur = rur./rr; vr = rvr./rr;
pr = (gamma-1)*(Er - 0.5*rr.*(ur.^2+vr.^2)); cr = sqrt(gamma*pr./rr);
F1r = rur; F2r = rur.*ur + pr; F3r = rur.*vr; F4r = (Er + pr).*ur;

% Compute dissipation for LF
maxvell = cl + abs(ul); maxvelr = cr + abs(ur); 
alpha = max(max(max(maxvell,maxvelr))); % Global LF
% alpha = max(maxvell,maxvelr); % Local LF

% Compute flux
Nq = size(ql); N = Nq(1); dq = zeros(N,4);
dq(:,1)=(F1l+F1r)/2-alpha/2.*(rr-rl);  
dq(:,2)= (F2l+F2r)/2-alpha/2.*(rur-rul);
dq(:,3)=(F3l+F3r)/2-alpha/2.*(rvr-rvl);
dq(:,4)=(F4l+F4r)/2-alpha/2.*(Er-El);
return