function Y = morley04(X,Y,U,COEFF)
% Morley's triangle, polynomial with design matrix 
% in (xi,eta) coordinates
% X,Y   : Coordinates of triangle
% U(1:2): argument of polynomial
% COEFF : Coefficients of polynomial

COEFF = COEFF(:);
X1 = X(1); X2 = X(2); X3 = X(3); Y1 = Y(1); Y2 = Y(2); Y3 = Y(3);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2;
X4 = (X1+X2)/2; X5 = (X2+X3)/2; X6 = (X3+X1)/2; 
Y4 = (Y1+Y2)/2; Y5 = (Y2+Y3)/2; Y6 = (Y3+Y1)/2; 
X13 = - X31; Y13 = - Y31; Y12 = - Y21;
DET = X21*Y31 - X31*Y21;
L1 = sqrt(X21^2 + Y21^2); L2 = sqrt(X32^2 + Y32^2);
L3 = sqrt(X31^2 + Y31^2);

C4 =   Y21/L1; S4 =  - X21/L1;
C5 =   Y32/L2; S5 =  - X32/L2;
C6 =   Y13/L3; S6 =  - X13/L3;
A4 = (C4*Y31 + S4*X13)/DET; B4 = (C4*Y12 + S4*X21)/DET;
A5 = (C5*Y31 + S5*X13)/DET; B5 = (C5*Y12 + S5*X21)/DET;
A6 = (C6*Y31 + S6*X13)/DET; B6 = (C6*Y12 + S6*X21)/DET;

BB_1 = [1,  0,  0,  0,           0,  0;
        1,  1,  0,  1,           0,  0;
        1,  0,  1,  0,           0,  1;
        0, A4, B4, A4,      0.5*B4,  0;
        0, A5, B5, A5, 0.5*(A5+B5), B5;
        0, A6, B6,  0,      0.5*A6, B6];
BB = inv(BB_1);

P = [1,U(1),U(2),U(1)^2,U(1)*U(2),U(2)^2];
AUX = P*BB;
Y = COEFF'*AUX';
