function Y = morley02(X,Y,U,COEFF)
% Morley's triangle, polynomial
% X,Y   : Coordinates of triangle
% U(1:2): argument of polynomial
% COEFF : Coefficients of polynomial
COEFF = COEFF(:);
X1 = X(1); X2 = X(2); X3 = X(3); Y1 = Y(1); Y2 = Y(2); Y3 = Y(3);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2; X23 = - X32;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2; Y23 = - Y32;
X4 = (X1+X2)/2; X5 = (X2+X3)/2; X6 = (X3+X1)/2; 
Y4 = (Y1+Y2)/2; Y5 = (Y2+Y3)/2; Y6 = (Y3+Y1)/2; 
X13 = - X31; Y13 = - Y31; Y12 = - Y21; X12 = - X21;
DET = X21*Y31 - X31*Y21;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555
Z1 = (X2*Y3 - X3*Y2 + Y23*U(1) + X32*U(2))/DET;
Z2 = (X3*Y1 - X1*Y3 + Y31*U(1) + X13*U(2))/DET;
Z3 = 1 - Z1 - Z2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
L1 = sqrt(X21^2 + Y21^2); L2 = sqrt(X32^2 + Y32^2);
L3 = sqrt(X31^2 + Y31^2);
a1 = (Y31*Y23 + X31*X23)/L3^2;   b1 = (Y12*Y23 + X12*X23)/L1^2;
a2 = (Y12*Y31 + X12*X31)/L1^2;   b2 = (Y23*Y31 + X23*X31)/L2^2;
a3 = (Y23*Y12 + X23*X12)/L2^2;   b3 = (Y31*Y12 + X31*X12)/L3^2;

N1 = Z1 + Z1*(Z1-1) + a1*Z2*(Z2-1) + b1*Z3*(Z3-1);
N2 = Z2 + Z2*(Z2-1) + a2*Z3*(Z3-1) + b2*Z1*(Z1-1);
N3 = Z3 + Z3*(Z3-1) + a3*Z1*(Z1-1) + b3*Z2*(Z2-1);
N4 = DET*Z3*(Z3 - 1)/L1;
N5 = DET*Z1*(Z1- 1)/L2;
N6 = DET*Z2*(Z2 - 1)/L3;
NN = [N1,N2,N3,N4,N5,N6];
Y = COEFF'*NN';






