function bild01
% Figure for DEMO1.M, Pitchfork bifurcation

load daten1b Y MU UU Parmeter
% -- Axes ---------
clf
c = 0.15; d = 0.05;
XX = [-0.5,3]; YY = [0,0];
arrow(XX,YY,c,d,'k',2)
XX = [0,0]; YY = [-1.5,1.5];
arrow(XX,YY,c,d,'k',2)
axis equal tight, axis manual
A = 1;
TT = linspace(1,3,40);
XX = sqrt(TT - A);
plot(TT,XX,'r'), hold on
plot(TT,-XX,'r'), hold on
plot(MU,Y,'k*')
