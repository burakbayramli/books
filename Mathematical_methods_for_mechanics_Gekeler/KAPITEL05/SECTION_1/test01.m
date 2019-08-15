function test01
% Calculation for example of Crandall

A = [1,0; 0,10];
x1 = 0; x2 = 3/sqrt(11); MU = 11/2;
gradf = [3*x1^2-1+x2^2, 2*x1*x2;
         -2*x1*x2, -(1+2*x1^2+3*x2^2)];
A + MU*gradf
