function demo5
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
clear, clc
disp(' Normal Offseting, nur DEMO-Programm ')
example = 100;
while ~ismember(example,[1,2])
   example = input(' Example No. (1/2) ');
end
switch example
case 1
   [p,e,segnr1,segnr2]  = bsp07;
   MAXIT  = 1; t = []; DIST = 0.5;
   KNOTEN =  mesh14(p,MAXIT);
   save daten p e t segnr1 segnr2 DIST
   save daten5a KNOTEN
   bild02
case 2
   [p,e,segnr1,segnr2]  = bsp08;
   MAXIT  = 2; t = []; DIST = 0.5;
   KNOTEN = mesh14(p,MAXIT);
   KNOTEN = [KNOTEN,[0;0;4]];
   save daten p e t segnr1 segnr2 DIST
   save daten5a KNOTEN
   bild02
end