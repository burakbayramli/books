function test02
% Testen von Geometrie und Randbedingungen
% fuer thermal lfow in a cup
clc
clf
RADIUS = sqrt(9 + 0.25);
WINKEL = atan(0.5/3);

hold on
   A1 = [2.5  3.5; 0  0];
   plot(A1(1,:),A1(2,:)), hold on

   NT = 30;  % hinreichend gross waehlen!
   TT = linspace(-0.5*pi + WINKEL,0,NT);
   A2  = [3 + RADIUS*cos(TT); 3 + RADIUS*sin(TT)];
   plot(A2(1,:),A2(2,:)), hold on

   A3 = [3+RADIUS, 3-RADIUS; 3  3];
   plot(A3(1,:),A3(2,:)), hold on

   NT = 30;  % hinreichend gross waehlen!
   TT = linspace(pi,1.5*pi-WINKEL,NT);
   A4  = [3 + RADIUS*cos(TT); 3 + RADIUS*sin(TT)];
   plot(A4(1,:),A4(2,:)), hold on

clear
