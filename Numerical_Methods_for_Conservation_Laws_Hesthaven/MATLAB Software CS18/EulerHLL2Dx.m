function [dq] = EulerHLL2Dx(ql,qr,gamma)
% function [dq] = EulerHLL2Dx(ql,qr,gamma)
% Purpose: Evaluate HLL numerical flux for Euler's equation along x

% Extract states
rl = ql(:,1); rul = ql(:,2); rvl = ql(:,3); El = ql(:,4);
rr = qr(:,1); rur = qr(:,2); rvr = qr(:,3); Er = qr(:,4);

% Compute fluxes for left and right states
ul = rul./rl; vl = rvl./rl;
pl = (gamma-1)*(El - 0.5*rl.*(ul.^2+vl.^2)); cl = sqrt(gamma*pl./rl);
F1l = rul; F2l = rul.*ul + pl; F3l = rul.*vl; F4l = (El + pl).*ul;
sl = min(ul,min(ul+cl,ul-cl)); 

ur = rur./rr; vr = rvr./rr;
pr = (gamma-1)*(Er - 0.5*rr.*(ur.^2+vr.^2)); cr = sqrt(gamma*pr./rr);
F1r = rur; F2r = rur.*ur + pr; F3r = rur.*vr; F4r = (Er + pr).*ur;
sr = max(ur,max(ur+cr,ur-cr));

% Compute Roe average along x and velocity bounds
rs = (sr.*rr - sl.*rl + F1l - F1r)./(sr-sl); 
rus = (sr.*rur - sl.*rul + F2l - F2r)./(sr-sl);
rvs = (sr.*rvr - sl.*rvl + F3l - F3r)./(sr-sl); 
Es = (sr.*Er - sl.*El + F4l - F4r)./(sr-sl);

us = rus./rs; vs = rvs./rs; 
ps = (gamma-1)*(Es - 0.5*rs.*(us.^2 + vs.^2)); cs = sqrt(gamma*ps./rs);

ssmi = min(us,min(us+cs,us-cs)); ssma = max(us,max(us+cs,us-cs)); 
sm = min(sl,ssmi); sp = max(sr,ssma);

% Compute flux
Nq = size(ql); N = Nq(1); dq = zeros(N,4);
q1 = (sm>0); q2 = (sp>=0).*(sm<=0); q3 = (sp<0);
fs = (sp.*F1l - sm.*F1r + sm.*sp.*(rr-rl))./(sp-sm); 
dq(:,1) = q1.*F1l + q2.*fs + q3.*F1r;
fs = (sp.*F2l - sm.*F2r + sm.*sp.*(rur-rul))./(sp-sm); 
dq(:,2) = q1.*F2l + q2.*fs + q3.*F2r;
fs = (sp.*F3l - sm.*F3r + sm.*sp.*(rvr-rvl))./(sp-sm); 
dq(:,3) = q1.*F3l + q2.*fs + q3.*F3r;
fs = (sp.*F4l - sm.*F4r + sm.*sp.*(Er-El))./(sp-sm); 
dq(:,4) = q1.*F4l + q2.*fs + q3.*F4r;
return