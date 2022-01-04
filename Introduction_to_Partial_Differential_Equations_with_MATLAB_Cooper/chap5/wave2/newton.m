

function uu = newton(ww,vv,vsr,vsl)
global alpha
global beta
global rho

global v

% first guess for uu

    K = length(v);
    uu = v(2:K-1);
    for i = 1:5
       uu = uu - l(uu,ww,vv,vsr, vsl)./dl(uu,ww);
    end
   
