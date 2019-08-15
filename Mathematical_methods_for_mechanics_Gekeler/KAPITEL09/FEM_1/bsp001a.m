function [p,e,t] = bsp001a
% Square with cavity, 4. Quadrant, coarse mesh
% Schwarz: beisp66.dat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
p = [0, 2, 5, 5, 0;
     0, 0, 3, 5, 5];  % Knoten

e = [1, 2, 3, 4, 5;
     2, 3, 4, 5, 1;
     0, 0, 0, 0, 0;
     1, 1, 1, 1, 1;
     1, 2, 3, 4, 5];  % edges

t = [1, 2, 3;         % elements
     2, 3, 4;
     5, 5, 5];
