%
% Problem 4.6 check on flap effectiveness
%
F = 0:.001:.5; c=1;
cosPhi = 2.*F-1;
Phi = acos(cosPhi);
a1 = 2*pi;
a2 = 2.*(pi - Phi + sin(Phi));
eff = a2./a1;
effCk = 4.*sqrt(F)./pi;
plot(F,eff,F,effCk,'o')