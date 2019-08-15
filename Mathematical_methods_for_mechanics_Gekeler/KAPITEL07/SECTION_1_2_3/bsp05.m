function [p,e,LAGER,LASTEN,PARMETER] = bsp05
% einfacher Balken

E   = 0.2e+09;  % Elastizitaetsmodul
NU  = 0.3;      % Poisson-Zahl
RHO = 1;
P   = 80;
PARMETER = [E, NU, RHO, P];
p = [0, 1, 2;
     0, 0, 0];
e = [1,       2;
     2,       3;
     0.08, 0.08;
     0.08, 0.08];

LASTEN      = zeros(3,size(p,2));
LASTEN(:,2) = [0; -P; 0];
%
% LAGER: SPALTE = [KNOTEN; U; V; Th];
LAGER = [1, 1, 3;
         1, 0, 0;
         0, 1, 1;
         0, 0, 0];
