function [UN4,UN5,UN6] = bell2(X,Y,COEFF)
% Computation of normal derivatives after Bell
% Hermite Interpolation see Math. Meth. Par. 7.3(b)
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
UNA  = C4*PX1 + S4*PY1; 
UNAx = C4*PXX1 + S4*PXY1; UNAy = C4*PXY1 + S4*PYY1;
UNAt = S4*UNAx - C4*UNAy; 
UNB  = C4*PX2 + S4*PY2;
UNBx = C4*PXX2 + S4*PXY2; UNBy = C4*PXY2 + S4*PYY2;
UNBt = S4*UNBx - C4*UNBy;
a = UNA; b = UNAt;
c = -(3*(UNA - UNB) + L1*(2*UNAt + UNBt))/L1^2; 
d = (2*(UNA- UNB) + L1*(UNAt + UNBt))/L1^3;
AUX = L1/2;
UN4 = ((d*AUX + c)*AUX + b)*AUX + a;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
UNA = C5*PX2 + S5*PY2;
UNAx = C5*PXX2 + S5*PXY2; UNAy = C5*PXY2 + S5*PYY2;
UNAt = S5*UNAx - C5*UNAy; 
UNB  = C5*PX3 + S5*PY3;
UNBx = C5*PXX3 + S5*PXY3; UNBy = C5*PXY3 + S5*PYY3;
UNBt = S5*UNBx - C5*UNBy;
a = UNA; b = UNAt;
c = -(3*(UNA - UNB) + L2*(2*UNAt + UNBt))/L2^2; 
d = (2*(UNA- UNB) + L2*(UNAt + UNBt))/L2^3;
AUX = L2/2;
UN5 = ((d*AUX + c)*AUX + b)*AUX + a;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
UNA = C6*PX3 + S6*PY3;
UNAx = C6*PXX3 + S6*PXY3; UNAy = C6*PXY3 + S6*PYY3;
UNAt = S6*UNAx - C6*UNAy; 
UNB  = C6*PX1 + S6*PY1;
UNBx = C6*PXX1 + S6*PXY1; UNBy = C6*PXY1 + S6*PYY1;
UNBt = S6*UNBx - C6*UNBy;
a = UNA; b = UNAt;
c = -(3*(UNA - UNB) + L3*(2*UNAt + UNBt))/L3^2; 
d = (2*(UNA- UNB) + L3*(UNAt + UNBt))/L3^3;
AUX = L3/2;
UN6 = ((d*AUX + c)*AUX + b)*AUX + a;
