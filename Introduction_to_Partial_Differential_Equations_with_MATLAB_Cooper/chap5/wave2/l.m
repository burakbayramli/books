

function y = l(uu,ww,vv,vsr,vsl)


global alpha
global beta
global rho


 y = uu + beta*(uu.^3 + uu.^2.*ww + uu.*ww.^2 + ww.^3) -rho*(vsr + vsl)- alpha*vv+ww;    
