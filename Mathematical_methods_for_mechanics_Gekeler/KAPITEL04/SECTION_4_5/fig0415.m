function fig0415a(nr)

% Bilder zum Raumgleiter X-38 mit SQP_H.m
switch nr
case 1, load daten12 X Parmeter
case 2, load daten13 X Parmeter
end
load daten12 X Parmeter
Parmtr1 = Parmeter(1:9);
n  = Parmtr1(1); BTA = Parmtr1(2); R = Parmtr1(4);
RRHO = Parmtr1(5); T_END = Parmtr1(6);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4:4*(n+1)); X5 = X(4*n+5:5*(n+1));
X6 = X(5*n+6:6*(n+1)); U = X(6*n+7:7*(n+1));
TT = linspace(0,T_END,n+1);
clf
subplot(3,1,1)
plot(TT,X4,'b','linewidth',2), hold on
grid off
title('\chi (rad)')
subplot(3,1,2)
plot(TT,X5,'b','linewidth',2), hold on
grid off
title('\lambda(rad)')
subplot(3,1,3)
plot(TT,X6,'b','linewidth',2), hold on
grid off
title('\tau (rad)')


