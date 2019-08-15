function [p,e,LAGER,LASTEN] = bsp01(Parmeter);
% p(1,:)         : Knoten, X-Komponente
% p(2,:)         : Knoten, Y-Komponente
% e(1,:)         : Staebe, Nr. des Anfangspunktes
% e(2,:)         : Staebe, Nr. des Endpunktes
% Lagergleichung : a*x_1 + b*x_2 = 0
%                  1 oder 2 Gleichungen
% Lagerform      : [Knotennr;a;b]
% LASTEN(1,:)    : Kraft, X-Komponente (alle Knoten)
% LASTEN(2,:)    : Kraft, Y-Komponente  (alle Knoten)
P = Parmeter(3); PHI = Parmeter(4);

p        = [
           % 1  2  3  4  5
             3, 2, 1, 0, 0;
             1, 0, 1, 0, 2];
e        = [
           % 1  2  3  4  5  6  7
             2, 3, 3, 4, 4, 5, 4;
             1, 1, 2, 2, 3, 3, 5];
LAGER    = [4, 4,        5;
            1, 0,-sin(PHI);
            0, 1, cos(PHI)];
LASTEN   = [ 0, 0, 0, 0, 0;
            -P, 0, 0, 0, 0];
