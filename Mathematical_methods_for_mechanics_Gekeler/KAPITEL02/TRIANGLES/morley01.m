function Y = morley01(X,Y,U,COEFF)
% Morley's triangle, polynomial with design matrix 
% in (x,y) coordinates
% X,Y   : Coordinates of triangle
% U(1:2): argument of polynomial
% COEFF : Coefficients of polynomial

COEFF = COEFF(:);
X1 = X(1); X2 = X(2); X3 = X(3); Y1 = Y(1); Y2 = Y(2); Y3 = Y(3);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2;
X4 = (X1+X2)/2; X5 = (X2+X3)/2; X6 = (X3+X1)/2; 
Y4 = (Y1+Y2)/2; Y5 = (Y2+Y3)/2; Y6 = (Y3+Y1)/2; 
X13 = - X31; Y13 = - Y31;
DET = X21*Y31 - X31*Y21;
L1 = sqrt(X21^2 + Y21^2); L2 = sqrt(X32^2 + Y32^2);
L3 = sqrt(X31^2 + Y31^2);

C4 =   Y21/L1; S4 =  - X21/L1;
C5 =   Y32/L2; S5 =  - X32/L2;
C6 =   Y13/L3; S6 =  - X13/L3;

BB_1 = [1, X1, Y1,    X1^2,       X1*Y1,    Y1^2;
      1, X2, Y2,    X2^2,       X2*Y2,    Y2^2;
      1, X3, Y3,    X3^2,       X3*Y3,    Y3^2;
      0, C4, S4, 2*C4*X4, S4*X4+C4*Y4, 2*S4*Y4;
      0, C5, S5, 2*C5*X5, S5*X5+C5*Y5, 2*S5*Y5;
      0, C6, S6, 2*C6*X6, S6*X6+C6*Y6, 2*S6*Y6];
BB = inv(BB_1);
B_1 = [  1,    0,    0,   0,    0,   0;
         1,    1,    0,   1,    0,   0;
         1,    0,    1,   0,    0,   1;
         0,    0,    1,   0,  1/2,   0;
         0,   -1,   -1,   -1,  -1,   -1;
         0,    1,    0,   0,    1/2,   0];
B_1(5,:) = B_1(5,:)/sqrt(2);         

P = [1,U(1),U(2),U(1)^2,U(1)*U(2),U(2)^2];
AUX = P*BB;
Y = COEFF'*AUX';
