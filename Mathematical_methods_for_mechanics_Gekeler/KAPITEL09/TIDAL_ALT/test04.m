function test04
% Testen der drei Matrizen N... Nino. S. 165
clc
syms x y
PSI1  = [1-x-y,x,y]; PSI2 = [1-x-y;x;y];
A = PSI2*PSI1;
B1 = (1-x-y)*A;
INTAB1 = int(B1,x,0,1-y);
INTBB1 = int(INTAB1,y,0,1);
C = simplify(INTBB1);
D = C*120;
D


