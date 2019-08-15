function [UN4,UN5,UN6] = bell(X,Y,COEFF)
% Computation of normal derivatives after Bell
% Formula after Ciarlet 1978, p. 72
% X,Y   : Coordinates of triangle
% COEFF : Coefficients of polynomial
COEFF = COEFF(:);
X1 = X(1); X2 = X(2); X3 = X(3); Y1 = Y(1); Y2 = Y(2); Y3 = Y(3);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2;
X4 = (X1+X2)/2; X5 = (X2+X3)/2; X6 = (X3+X1)/2; 
Y4 = (Y1+Y2)/2; Y5 = (Y2+Y3)/2; Y6 = (Y3+Y1)/2; 
X13 = - X31; Y13 = - Y31; X12 = - X21; Y12 = - Y21;
X23 = - X32; Y23 = - Y32;
DET = X21*Y31 - X31*Y21;
L1 = sqrt(X21^2 + Y21^2); L2 = sqrt(X32^2 + Y32^2);
L3 = sqrt(X31^2 + Y31^2);
PX1 = COEFF(2); PY1 = COEFF(3);
PXX1 = COEFF(4);PXY1 = COEFF(5); PYY1 = COEFF(6);
PX2 = COEFF(8); PY2 = COEFF(9);
PXX2 = COEFF(10);PXY2 = COEFF(11); PYY2 = COEFF(12);
PX3 = COEFF(14); PY3 = COEFF(15);
PXX3 = COEFF(16);PXY3 = COEFF(17); PYY3 = COEFF(18);

C4 =  - Y21/L1; S4 =   X21/L1;
C5 =  - Y32/L2; S5 =   X32/L2;
C6 =  - Y13/L3; S6 =   X13/L3;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
UN1 = C4*PX1 + S4*PY1;
UN2 = C4*PX2 + S4*PY2;
GRADUN1 = [C4*PXX1 + S4*PXY1, C4*PXY1 + S4*PYY1];
GRADUN2 = [C4*PXX2 + S4*PXY2, C4*PXY2 + S4*PYY2];
UN4 = (UN1 + UN2)/2 + (GRADUN1*[X21;Y21] + GRADUN2*[X12;Y12])/8;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
UN1 = C5*PX2 + S5*PY2;
UN2 = C5*PX3 + S5*PY3;
GRADUN1 = [C5*PXX2 + S5*PXY2, C5*PXY2 + S5*PYY2];
GRADUN2 = [C5*PXX3 + S5*PXY3, C5*PXY3 + S5*PYY3];
UN5 = (UN1 + UN2)/2 + (GRADUN1*[X32;Y32] + GRADUN2*[X23;Y23])/8;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
UN1 = C6*PX3 + S6*PY3;
UN2 = C6*PX1 + S6*PY1;
GRADUN1 = [C6*PXX3 + S6*PXY3, C6*PXY3 + S6*PYY3];
GRADUN2 = [C6*PXX1 + S6*PXY1, C6*PXY1 + S6*PYY1];
UN6 = (UN1 + UN2)/2 + (GRADUN1*[X13;Y13] + GRADUN2*[X31;Y31])/8;


