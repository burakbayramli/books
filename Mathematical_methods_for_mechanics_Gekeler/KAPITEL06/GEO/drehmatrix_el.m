function [D1ALF,D2BET,D3GAM] = drehmatrix_el(ALF,BET,GAM)
% D1ALF*X rotates X about [1;0;0] with angle ALF in pos. direction 
% D2BET*X rotates X about [0;1;0] with angle BET in pos. direction 
% D3GAM*X rotates X about [0;0;1] with angle GAM in pos. direction 

D1ALF = [1,        0,         0;
         0, cos(ALF), -sin(ALF);
         0, sin(ALF),  cos(ALF)];
D2BET = [cos(BET), 0, -sin(BET);
                0, 1,         0;
         sin(BET), 0, cos(BET)];
D3GAM = [cos(GAM), -sin(GAM), 0;
         sin(GAM),  cos(GAM), 0;
                0,         0, 1];
