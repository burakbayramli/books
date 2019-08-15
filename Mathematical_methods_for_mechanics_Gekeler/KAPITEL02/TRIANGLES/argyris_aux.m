function Y = argyris_aux1(C1,C2,C3,COEFF)
% Interpolation polynomial for Argyris triangle
% in area coordinates (for integration)
% as ARGYRIS04.M, for general triangle
% superfluous shape functions deleted
% X,Y   : Coordinates of triangle
% U(1:2): argument of polynomial
% COEFF : Coefficients of polynomial
% Reihenfolge:

X1 = C1(1); Y1 = C1(2); X2 = C2(1); Y2 = C2(2); X3 = C3(1); Y3 = C3(2);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2;
X4 = (X1+X2)/2; X5 = (X2+X3)/2; X6 = (X3+X1)/2; 
Y4 = (Y1+Y2)/2; Y5 = (Y2+Y3)/2; Y6 = (Y3+Y1)/2; 
X12 = -X21; Y12 = -Y21; X13 = -X31; Y13 = -Y31; X23 = -X32; Y23 = -Y32;
DET = X21*Y31 - X31*Y21;

MU1 = C1(3); MU2 = C2(3); MU3 = C3(3);
z1 = C1(4); z2 = C2(4); z3 = C3(4);

COEFF = COEFF(:);

N1 = z1*(1+z2*(1+3*z1*z2)*(z1-z2)+z3*(1+3*z3*z1)*(z1-z3) ...
     + 2*z2*z3*(3*z1- 1)) - 15*MU1*z1^2*z2^2*z3 + 15*MU3*z1^2*z2*z3^2; 
     
% 1. Ecke: 1. Ableitungen      
N3   = z1*z3*(1+z1-z3+z1*(3*z2+z3)-z2*(z2+z3)+3*z1*z3*(z1-z3)) ...
       -16*z1^2*z2^2*z3 +(8 + 7*MU3)*z1^2*z2*z3^2;
N3   = N3/2;
N2   = z1*z2*(1+z1-z2+z1*(3*z3+z2)-z3*(z2+z3)+3*z1*z2*(z1-z2)) ...
       + (8 - 7*MU1)*z1^2*z2^2*z3 - 16*z1^2*z2*z3^2;
N2   = N2/2;
% -- 1. Ecke: 2. Ableitungen nach x und y ------------------------
N6   = z3^2*z1^2*(1+z1-z3) + (4 + MU3)*z1^2*z2*z3^2;          N6 = N6/4;
N5   = 4*z1^2*z2*z3        - 8*z1^2*z2^2*z3 - 8*z1^2*z2*z3^2; N5 = N5/4;
N4   = z1^2*z2^2*(1+z1-z2) + (4 - MU1)*z1^2*z2^2*z3;          N4 = N4/4;

PP1 = COEFF(1)*N1 + [COEFF(2),COEFF(3)]*([X31;Y31]*N3 + [X21;Y21]*N2);
AA = [COEFF(4), COEFF(5); COEFF(5), COEFF(6)];
AUX4a = [X31,Y31]*AA*[X31;Y31]; AUX4b = [X21,Y21]*AA*[X31;Y31];
AUX4c = [X21,Y21]*AA*[X21;Y21];
AUX4 = AUX4a*N6 + AUX4b*N5 + AUX4c*N4;
Y = PP1 + AUX4;

  