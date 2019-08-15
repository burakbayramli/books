function fig0415_3
% Figure 4.15c, Space glider X-38 with SQP_H.m

disp(' Call first DEMO4 wit one restart ')
load daten12 X Parmeter
Parmtr1 = Parmeter(1:9);
clf
n  = Parmtr1(1); BTA = Parmtr1(2); R = Parmtr1(4);
RRHO = Parmtr1(5); T_END = Parmtr1(6);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4:4*(n+1)); X5 = X(4*n+5:5*(n+1));
X6 = X(5*n+6:6*(n+1)); U = X(6*n+7:7*(n+1));
TT = linspace(0,T_END,n+1);
clf
plot(TT,X6,'k','linewidth',2), hold on
grid off
title('\tau (rad)','fontsize',40)
