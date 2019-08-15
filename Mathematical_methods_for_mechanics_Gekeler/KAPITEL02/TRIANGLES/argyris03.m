function YY = argyris03(X,Y,U,COEFF)
% Interpolation polynomial for Argyris triangle
% in area coordinates (for integration)
% Cyclic representation for check
% X,Y   : Coordinates of triangle
% U(1:2): argument of polynomial
% COEFF : Coefficients of polynomial
% Reihenfolge:
% Ecke 1: [u,u_x,u_y,u_xx,u_xy,u_yy] := [N1 , N2, N3, N4, N5, N6]
% Ecke 2: [u,u_x,u_y,u_xx,u_xy,u_yy] := [N7 , N8, N9,N10,N11,N12]
% Ecke 3: [u,u_x,u_y,u_xx,u_xy,u_yy] := [N13,N14,N15,N16,N17,N18]
% Mittelpunkte   1-2,2-3,3-1: N19, N20, N21

%z2 = U(1); z3 = U(2); z1 = 1 - z2 - z3;
COEFF = COEFF(:);
X1  = X(1); X2 = X(2); X3 = X(3); Y1 = Y(1); Y2 = Y(2); Y3 = Y(3);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2;
X4  = (X1+X2)/2; X5 = (X2+X3)/2; X6 = (X3+X1)/2; 
Y4  = (Y1+Y2)/2; Y5 = (Y2+Y3)/2; Y6 = (Y3+Y1)/2; 
X12 = -X21; Y12 = -Y21; X13 = -X31; Y13 = -Y31; X23 = -X32; Y23 = -Y32;
DET = X21*Y31 - X31*Y21;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
z1 = (X2*Y3 - X3*Y2 + Y23*U(1) + X32*U(2))/DET;
z2 = (X3*Y1 - X1*Y3 + Y31*U(1) + X13*U(2))/DET;
z3 = 1 - z1 - z2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Lengths of edges ------------------
L1  = sqrt(X21^2 + Y21^2); L2 = sqrt(X32^2 + Y32^2);
L3  = sqrt(X31^2 + Y31^2);
% Correcture factors after Agryris and Zienkiewicz

MU1 = (L3^2 - L2^2)/L1^2; MU2 = (L1^2 - L3^2)/L2^2;
MU3 = (L2^2 - L1^2)/L3^2;

C1 = [X1,Y1,MU1,z1]; C2 = [X2,Y2,MU2,z2];
C3 = [X3,Y3,MU3,z3];

Y2A = argyris_aux(C1,C2,C3,COEFF(1:6));
Y2B = argyris_aux(C2,C3,C1,COEFF(7:12));
Y2C = argyris_aux(C3,C1,C2,COEFF(13:18));

% -- Weights and shape functions for normal derivatives
a19 = DET/L1; a20 = DET/L2; a21 = DET/L3; 
N19 = z1^2*z2^2*z3; N20 = z1*z2^2*z3^2; N21 = z1^2*z2*z3^2;
NNAUX = [a19*16*N19;a20*16*N20;a21*16*N21];
% -- alternatively with Bell's triangle ------------------
% Normal derivatives at midpoints are CALCULATED
% Two different approaches for check
Bell = 0;
switch Bell
case 0
   Y2D = COEFF([19,20,21])'*NNAUX;
case 1
   [UN4,UN5,UN6] = bell1(X,Y,COEFF);
   [UN4A,UN5A,UN6A] = bell2(X,Y,COEFF); % alternative
   DIFF = norm([UN4,UN5,UN6] - [UN4A,UN5A,UN6A])
   Y2D = [UN4,UN5,UN6]*NNAUX;
end
YY = Y2A + Y2B + Y2C + Y2D;
