function bild02a
% Image for shallow water in a Channel
%clc
load daten2a p e t
load daten2b U V Z PUNKTA PUNKTB
% -- Eckpunkte ------------------
clf, hold on
MAXAMPLITUDE = max(abs(Z));
plot(0,-6,'w.'), hold on
plot(5,6,'w.'), hold on
axis tight, axis manual, grid on
% -- Figure -----------------
XX = [0,1,2,3,4,5];
MONITOR = [14,15,16,17,18,7];
RZ = Z(7); RX = 6;
ZZ = Z(MONITOR);  UU = U(MONITOR);
plot(XX,ZZ,'r','linewidth',1), hold on
plot(XX(RX),RZ,'ro'), hold on
plot(XX,UU,'g','linewidth',2), hold on
plot(XX,0*XX,'k','linewidth',2), hold on

