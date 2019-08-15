function [F,FD,FDD] = bsp01b(X,S);
% Calculates flow velocity for Zermelo's problem
% FDD = [v_11, v_12; v_21, v_22];
F   = [- S*X(2); 0];
FD  = [0, -S;
       0, 0];
FDD = zeros(2,2);
