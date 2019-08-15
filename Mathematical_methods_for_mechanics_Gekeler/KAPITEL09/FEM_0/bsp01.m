function [p,e,t] = bsp01
% Quadrat, Grobes  Dreiecksgitter
% fuer lid driven cavity

p = [0, 1, 1, 0, 0.5;
     0, 0, 1, 1, 0.5]; 

t = [1, 2, 3, 4;
     2, 3, 4, 1;
     5, 5, 5, 5];

e = [1, 2, 3, 4;
     2, 3, 4, 1;
     0, 0, 0, 0;
     1, 1, 1, 1;
     1, 2, 3, 4];

