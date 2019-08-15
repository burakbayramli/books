function demo6
% Eckart Gekeler, Universitaet Stuttgart, Release 14.4.05
clear, clc
disp(' Manuelle Verschiebung und Glaettung, Bildfolge ')
disp('Fuer DELAUNAY.M PDE-TOOLBOX notwendig ')

[p,e,t,segnr1,segnr2] = bsp06;
DIST = 0.5;
bild01(p,e,t,segnr1,segnr2,DIST);
disp('Weiter mit bel. Taste')
pause
disp('Knoten in das Zentrum des umgebenden Polygons verschieben:')
p = mesh10(p,e,t);
bild01(p,e,t,segnr1,segnr2,DIST);
disp('Weiter mit bel. Taste')
pause
disp('Knoten manuell verschieben:')
disp(' Weiter: Auf Rand tippen! ')
p = mesh02(p,e,t);
bild01(p,e,t,segnr1,segnr2,DIST);
disp('Weiter mit bel. Taste')
pause
disp('Lange Kanten durch kurze ersetzen:')
GRAFIK = 1;
t = mesh03(p,t,GRAFIK);
bild01(p,e,t,segnr1,segnr2,DIST);
disp('Weiter mit bel. Taste')
pause
disp('Delaunay-Triangulierung')
t  = delaunay(p(1,:),p(2,:));
t =  t';
flipud(t);
bild01(p,e,t,segnr1,segnr2,DIST);
disp('Weiter mit bel. Taste')
pause
disp('Elemente ausserhalb eliminieren')
segnr1 = 1; segnr2 = [];
t = mesh27(p,e,t,segnr1,segnr2);
bild01(p,e,t,segnr1,segnr2,DIST);
