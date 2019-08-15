function demo3
% Eckart Gekeler, Universitaet Stuttgart, Release 14.4.05
clear, clc
disp(' Seitenmitten und Normalen ')

[p,e,t,segnr1,segnr2] = bsp06;
disp(' Refinemesh ')
[p,e,t] = mesh01(p,e,t);
disp(' Jigglemesh ')
p = mesh10(p,e,t);
[midpoints,normalen] = mesh13(p,t);
% -- Grafik ----------------
DIST = 0.5;
bild01(p,e,t,segnr1,segnr2,DIST)
X = [midpoints(1,:),midpoints(3,:), midpoints(5,:)];
Y = [midpoints(2,:), midpoints(4,:), midpoints(6,:)];
U = [normalen(1,:), normalen(3,:), normalen(5,:)];
V = [normalen(2,:), normalen(4,:), normalen(6,:)];
quiver(X,Y,U,V,0.2), hold on
plot(X,Y,'.','Markersize',3), hold on
