function [p,e,q] = bsp002g
% bilineares Parallelogrammelemente
% Einheitsquadrat mit 9 aequidist. Knoten
% Zum Testen: TSTFKT1.M
p = [0,  0.5,  1, 1  ,  1, 0.5, 0,  0  ,  0.5;
     0,  0  ,  0, 0.5,  1,   1, 1,  0.5,  0.5];
ELEMENTE = [1  2  9  8; 2  3  4  9; 9  4  5  6; 8  9  6  7];
q = ELEMENTE';
e1 =  [1, 2; 2, 3]; e2 = [3, 4; 4, 5]; e3 = [5, 6; 6, 7]; e4 = [7 ,8; 8, 1];
e1 = [e1; [0,1/2;1/2,1]; ones(1,2)];
e2 = [e2; [0,1/2;1/2,1]; 2*ones(1,2)];
e3 = [e3; [0,1/2;1/2,1]; 3*ones(1,2)];
e4 = [e4; [0,1/2;1/2,1]; 4*ones(1,2)];
e = [e1, e2, e3, e4];
