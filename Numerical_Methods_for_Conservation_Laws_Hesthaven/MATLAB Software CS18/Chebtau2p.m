function tau2p = Chebtau2p(x,p);
% function tau2p = Chebtau2p(x,p);
% Purpose: Evaluate Chebyshev abs approximation
% Note: x can be a matrix
tau2p = 2/pi + 0*x;
for k=1:p
    tau2p = tau2p + 4/pi*(-1)^(k+1)./(2*k-1)./(2*k+1).*cos(2*k*acos(x));
end
return