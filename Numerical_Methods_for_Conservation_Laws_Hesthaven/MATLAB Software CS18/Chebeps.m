function Ceps = Chebeps(p);
% function Ceps = Chebeps(p);
% Purpose: Compute shift of Chebychev based approximation to absolut value
% to ensure stability.

xs = fzero(@(x) Chebtaup(x,p)-1,0); Ceps = abs(xs) - abs(Chebtau2p(xs,p));
return

function taup = Chebtaup(x,p);
% function taup = Chebtaup(x,p);
% Purpose: Evaluate derivative of abs approximation to find shift
taup = 0;
for k=1:p
   taup = taup + 4/pi*(-1)^(k+1)./(2*k-1)./(2*k+1) ...
              .*2*k*sin(2*k*acos(x))./sqrt(1-x^2);
end
return