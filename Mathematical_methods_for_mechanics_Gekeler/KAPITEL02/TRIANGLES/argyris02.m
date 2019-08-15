function Y = argyris02(X,Y,U,COEFF)
% Interpolation polynomial for Argyris triangle
% in area coordinates (for integration)
% X,Y   : Coordinates of triangle
% U(1:2): argument of polynomial
% COEFF : Coefficients of polynomial
% Reihenfolge:
% Ecke 1: [u,u_x,u_y,u_xx,u_xy,u_yy] := [N1 , N2, N3, N4, N5, N6]
% Ecke 2: [u,u_x,u_y,u_xx,u_xy,u_yy] := [N7 , N8, N9,N10,N11,N12]
% Ecke 3: [u,u_x,u_y,u_xx,u_xy,u_yy] := [N13,N14,N15,N16,N17,N18]
% Mittelpunkte   1-2,2-3,3-1: N19, N20, N21
% N14M statt N14 etc. bedeutet modifizierte Formfunktion

%z2 = U(1); z3 = U(2); z1 = 1 - z2 - z3;
COEFF = COEFF(:);
X1 = X(1); X2 = X(2); X3 = X(3); Y1 = Y(1); Y2 = Y(2); Y3 = Y(3);
X21 = X2 - X1; X31 = X3 - X1; X32 = X3 - X2;
Y21 = Y2 - Y1; Y31 = Y3 - Y1; Y32 = Y3 - Y2;
X4 = (X1+X2)/2; X5 = (X2+X3)/2; X6 = (X3+X1)/2; 
Y4 = (Y1+Y2)/2; Y5 = (Y2+Y3)/2; Y6 = (Y3+Y1)/2; 
X12 = -X21; Y12 = -Y21; X13 = -X31; Y13 = -Y31; X23 = -X32; Y23 = -Y32;
DET = X21*Y31 - X31*Y21;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555
z1 = (X2*Y3 - X3*Y2 + Y23*U(1) + X32*U(2))/DET;
z2 = (X3*Y1 - X1*Y3 + Y31*U(1) + X13*U(2))/DET;
z3 = 1 - z1 - z2;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


L1 = sqrt(X21^2 + Y21^2); L2 = sqrt(X32^2 + Y32^2);
L3 = sqrt(X31^2 + Y31^2);
MU1 = (L3^2 - L2^2)/L1^2; MU2 = (L1^2 - L3^2)/L2^2;
MU3 = (L2^2 - L1^2)/L3^2;

% -- Coefficients of normals of edges: -------  
C4 =  - Y21/L1; S4 =   X21/L1; C5 =  - Y32/L2; S5 =   X32/L2;
C6 =  - Y13/L3; S6 =   X13/L3;

a1 = -15*MU1;  b1 = 0;  c1 = 15*MU3;
a2 = 8 -7*MU1; b2 = 0;  c2 = - 16;
a3 = - 16; b3 = 0; c3 = 8 + 7*MU3;
a4 = 4 - MU1; b4 = 0; c4 = 0;
a5 = - 8; b5 = 0; c5 = - 8;
a6 = 0; b6 = 0; c6 = 4 + MU3;
a7 = 15*MU1; b7 = -15*MU2; c7 = 0;
a8 = 8 + 7*MU1; b8 = - 16; c8 = 0;
a9 = - 16; b9 = 8 - 7*MU2; c9 = 0;

a10 = 4 + MU1; b10 = 0; c10 = 0;
a11 = - 8; b11 = - 8; c11 = 0;
a12 = 0; b12 = 4 - MU2; c12 = 0;
a13 = 0; b13 = 15*MU2; c13 = -15*MU3;
a14 = 0; b14 = - 16; c14 = 8 - 7*MU3;
a15 = 0; b15 = 8 + 7*MU2; c15 = - 16;
a16 = 0; b16 = 0; c16 = 4 - MU3;
a17 = 0; b17 = - 8; c17 = - 8;
a18 = 0; b18 = 4 + MU2; c18 = 0; 
N19 = z1^2*z2^2*z3; N20 = z1*z2^2*z3^2; N21 = z1^2*z2*z3^2;
% -- Fuer Funktionswerte ---------------------
N1 = z1*(1+z2*(1+3*z1*z2)*(z1-z2)+z3*(1+3*z3*z1)*(z1-z3) ...
     - 2*z2*z3*(z2+z3)+4*z1*z2*z3) + a1*N19 + c1*N21; 
N7  = z2*(1+z3*(1+3*z2*z3)*(z2-z3)+z1*(1+3*z1*z2)*(z2-z1) ...
     - 2*z3*z1*(z3+z1)+4*z1*z2*z3) + a7*N19 + b7*N20; 
N13 = z3*(1+z1*(1+3*z1*z3)*(z3-z1)+z2*(1+3*z2*z3)*(z3 -z2) ...
     - 2*z1*z2*(z1+z2)+4*z1*z2*z3) + b13*N20 + c13*N21; 
% -- 1. Ecke: Ableitung nach x und y ------------------------
N2   = z1*z2*(1+z1-z2+z1*(3*z3+z2)-z3*(z2+z3)+3*z1*z2*(z1-z2)) ...
       + a2*N19+c2*N21;
N2   = N2/2;
N3   = z1*z3*(1+z1-z3+z1*(3*z2+z3)-z2*(z2+z3)+3*z1*z3*(z1-z3)) ...
       + a3*N19+c3*N21;
N3   = N3/2;
% -- 2. Ecke: Ableitung nach x und y ------------------------
N8M = z2*z1*(1+z2-z1+z2*(3*z3+z1)-z3*(z3+z1)+3*z2*z1*(z2-z1)) ...
      + a8*N19+b8*N20;
N8M = N8M/2;
N9  = z2*z3*(1+z2-z3+z2*(3*z1+z3)-z1*(z3+z1)+3*z2*z3*(z2-z3)) ...
      + a9*N19+b9*N20;
N9  = N9/2;
% -- 3. Ecke: Ableitung nach x und y ------------------------
N14M = z3*z1*(1+z3-z1+z3*(3*z2+z1)-z2*(z2+z1)+3*z3*z1*(z3-z1)) ...
       + b14*N20+c14*N21;
N14M = N14M/2;
N15M = z3*z2*(1+z3-z2+z3*(3*z1+z2)-z1*(z2+z1)+3*z3*z2*(z3-z2)) ... 
       + b15*N20+c15*N21;
N15M = N15M/2;
% -- 1. Ecke: 2. Ableitungen nach x und y ------------------------
N4   = z1^2*z2^2*(1+z1-z2) + a4*N19;        N4 = N4/4;
N5   = 4*z1^2*z2*z3        + a5*N19+c5*N21; N5 = N5/4;
N6   = z3^2*z1^2*(1+z1-z3) + c6*N21;        N6 = N6/4;
% -- 2. Ecke: 2. Ableitungen nach x und y ------------------------
N10M = z1^2*z2^2*(1+z2-z1) + a10*N19;          N10M = N10M/4;
N11M = 4*z1*z2^2*z3        + a11*N19+ b11*N20; N11M = N11M/4;
N12 = z2^2*z3^2*(1+z2-z3)  + b12*N20;          N12 = N12/4;
% -- 3. Ecke: 2. Ableitungen nach x und y ------------------------
N16M = z3^2*z1^2*(1+z3-z1)     + c16*N21;           N16M = N16M/4; 
N17M = 4*z1*z2*z3^2            + b17*N20 + c17*N21; N17M = N17M/4; 
N18M = z2^2*z3^2*(1 + z3 - z2) + b18*N20;           N18M = N18M/4;

PP1 = COEFF(1)*N1 + COEFF(7)*N7 + COEFF(13)*N13;
AUX1 = [COEFF(2),COEFF(3)]*([X21;Y21]*N2 + [X31;Y31]*N3);
AUX2 = [COEFF(8),COEFF(9)]*([X12;Y12]*N8M + [X32;Y32]*N9);
AUX3 = [COEFF(14),COEFF(15)]*([X13;Y13]*N14M + [X23;Y23]*N15M);

AA = [COEFF(4), COEFF(5); COEFF(5), COEFF(6)];
AUX4a = [X21,Y21]*AA*[X21;Y21]; AUX4b = [X21,Y21]*AA*[X31;Y31];
AUX4c = [X31,Y31]*AA*[X31;Y31];
AUX4 = AUX4a*N4 + AUX4b*N5 + AUX4c*N6;

AA = [COEFF(10), COEFF(11); COEFF(11), COEFF(12)];
AUX5a = [X12,Y12]*AA*[X12;Y12]; AUX5b = [X12,Y12]*AA*[X32;Y32];
AUX5c = [X32,Y32]*AA*[X32;Y32];
AUX5 = AUX5a*N10M + AUX5b*N11M + AUX5c*N12;

AA = [COEFF(16), COEFF(17); COEFF(17), COEFF(18)];
AUX6a = [X13,Y13]*AA*[X13;Y13]; AUX6b = [X13,Y13]*AA*[X23;Y23];
AUX6c = [X23,Y23]*AA*[X23;Y23];
AUX6 = AUX6a*N16M + AUX6b*N17M + AUX6c*N18M;

PP1 = PP1 + AUX1 + AUX2 + AUX3 + AUX4 + AUX5 + AUX6;

a19 = DET/L1; a20 = DET/L2; a21 = DET/L3; 
MM = [19,20,21]; NNAUX = [a19*16*N19,a20*16*N20,a21*16*N21];
PP2 = COEFF(MM)'*NNAUX.';
Y = PP1 + PP2;

  