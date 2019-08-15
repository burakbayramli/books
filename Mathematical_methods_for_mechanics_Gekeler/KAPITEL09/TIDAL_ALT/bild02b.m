function bild02b
% Image for channel, Einzelne Wasserstaende
%clc
load daten2c PUNKTA PUNKTB
% -- Eckpunkte
clf, hold on
plot(0,-10,'w.'), hold on
plot(60,10,'w.'), hold on
axis equal tight, axis manual, grid on
plot(PUNKTA,'k'), hold on
plot(PUNKTB,'r'), hold on
%grid off
clear
