function [p,e,LAGER,LASTEN] = bsp02(PARMETER);

% Vgl.Gross et al. TM I;UE, S. 64
% p(1,:)         : Knoten, X-Komponente
% p(2,:)         : Knoten, Y-Komponente
% e(1,:)         : Staebe, Nr. des Anfangspunktes
% e(2,:)         : Staebe, Nr. des Endpunktes
% Lagergleichung : a*x_1 + b*x_2 = 0
%                  1 oder 2 Gleichungen
% Lagerform      : [Knotennr;a;b]
% LASTEN(1,:)    : Kraft, X-Komponente (alle Knoten)
% LASTEN(2,:)    : Kraft, Y-Komponente  (alle Knoten)

P   = PARMETER(3);
p        = [
           % 1  2  3  4  5  6
             0, 2, 4, 6, 4, 2;
             0, 0, 0, 0, 1, 1];
e        = [
           % 1  2  3  4  5  6  7  8  9
             1, 2, 3, 4, 5, 6, 6, 5, 6;
             2, 3, 4, 5, 6, 1, 2, 3, 3];
LAGER    = [1,  1,  4;
            1,  0,  0;
            0,  1,  1];
LASTEN   = [ 0,    0, 0, 0, P, 0;
             0, -2*P, 0, 0, 0, 0];
