function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
clear, clc, format short, format compact
disp(' Triangulierung ohne innere Punkte, Bildfolge ')
% segnr1 : Segmentnrn des Aussenrandes
% segnr2 : Segmentnrn des Innenrandes (ev. [])
set(gcf,'doublebuffer','on')
example= 100;
while ~ismember(example,[1,2,3,4])
   example = input(' Example No. (1/2/3/4) ');
end
switch example
case 1, disp(' Letters F E M, Bildfolge ')
   DIST = 0.5;  % Abstand zum Rand
   [p,e,segnr1,segnr2] = bsp01; t = [];
   bild01(p,e,t,segnr1,segnr2,DIST)
   GRAFIK = 1;
   [t,FORMED] = mesh11(p,e,segnr1,segnr2,GRAFIK);
   disp(' Refinemesh ')
   [p,e,t] = mesh01(p,e,t);
   disp(' Jigglemesh ')
   p = mesh10(p,e,t);
   bild01(p,e,t,segnr1,segnr2,DIST)
case 2, disp(' Rectangle with cavity, Bildfolge ')
   DIST = 0.5;
   [p,e,segnr1,segnr2] = bsp02; t = [];
   bild01(p,e,t,segnr1,segnr2,DIST)
   GRAFIK = 1;
   [t,FORMED] = mesh11(p,e,segnr1,segnr2,GRAFIK);
   bild01(p,e,t,segnr1,segnr2,DIST);
case 3, disp(' L-shape with cavity, Bildfolge ')
   DIST = 0.5;
   [p,e,segnr1,segnr2] = bsp03; t = [];
   bild01(p,e,t,segnr1,segnr2,DIST)
   GRAFIK = 1;
   [t,FORMED] = mesh11(p,e,segnr1,segnr2,GRAFIK);
   bild01(p,e,t,segnr1,segnr2,DIST);
case 4, disp(' Ring, Bildfolge ')
   DIST = 0.5;
   [p,e,segnr1,segnr2] = bsp09; t = [];
   bild01(p,e,t,segnr1,segnr2,DIST)
   GRAFIK = 1;
   [t,FORMED] = mesh11(p,e,segnr1,segnr2,GRAFIK);
   bild01(p,e,t,segnr1,segnr2,DIST);
end
