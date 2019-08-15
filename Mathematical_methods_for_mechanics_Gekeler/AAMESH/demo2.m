function demo2
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% DEMO fuer mesh12.m
% innere Dreiecke werden zu Vierecken zusammengelegt
% hier nur Quadrate (TODO)

clear, clc
disp(' Quadrate und Dreiecke, uses PDE-TOOLBOX ')
% segnr1 : Segmentnrn des Aussenrandes
% segnr2 : Segmentnrn des Innenrandes (ev. [])
example = 100;
while ~ismember(example,[1,2])
   example = input(' Example No. (1/2) ');
end
switch example
   case 1, [p,e,segnr1,segnr2] = bsp04;
   case 2, [p,e,segnr1,segnr2] = bsp05;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
N = 8; % Aufteilung fuer Quadrate Anpassen!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p     = mesh08(p,e,N,segnr1,segnr2);
t     = mesh09(p,e,segnr1,segnr2);
[t,q] = mesh12(p,e,segnr1,segnr2);
% -- Grafik -------------------
DIST = 0.5;
bild01(p,e,t,segnr1,segnr2,DIST)
M = size(q,2);
for I = 1:M
   quad = q(:,I); quad = [quad; quad(1)];
   X2 = p(1,quad); Y2 = p(2,quad);
   plot(X2,Y2,'b'), hold on
   plot(X2,Y2,'b.','markersize',6)
end
