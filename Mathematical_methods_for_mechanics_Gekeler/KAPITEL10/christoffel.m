function christoffel
% Kugelkoordinaten, Metriktensor und Christoffelsymbole
clc
% u = r, v = theta, w = phi
syms u v w
X =  [u*sin(v)*cos(w); u*sin(v)*sin(w); u*cos(v)];
G1 = diff(X,'u');
G2 = diff(X,'v');
G3 = diff(X,'w');
GRAD = [G1,G2,G3];
GRAD_T = GRAD.';
MM = GRAD_T*GRAD;
MM = simplify(MM);
H1 = G1.'/MM(1,1); % g^1
H2 = G2.'/MM(2,2); % g^2
H3 = G3.'/MM(3,3);  % g^3
HH = [H1;H2;H3];
%CC = HH*GRAD;
%CC = simplify(CC);
G1D1 = diff(G1,'u'); G1D2 = diff(G1,'v'); G1D3 = diff(G1,'w');
G2D1 = diff(G2,'u'); G2D2 = diff(G2,'v'); G2D3 = diff(G2,'w');
G3D1 = diff(G3,'u'); G3D2 = diff(G3,'v'); G3D3 = diff(G3,'w');
C1_11 = H1*G1D1; C1_12 = H1*G1D2; C1_13 = H1*G1D3;
C1_21 = H1*G2D1; C1_22 = H1*G2D2; C1_23 = H1*G2D3;
C1_31 = H1*G3D1; C1_32 = H1*G3D2; C1_33 = H1*G3D3;

C2_11 = H2*G1D1; C2_12 = H2*G1D2; C2_13 = H2*G1D3;
C2_21 = H2*G2D1; C2_22 = H2*G2D2; C2_23 = H2*G2D3;
C2_31 = H2*G3D1; C2_32 = H2*G3D2; C2_33 = H2*G3D3;

C3_11 = H3*G1D1; C3_12 = H3*G1D2; C3_13 = H3*G1D3;
C3_21 = H3*G2D1; C3_22 = H3*G2D2; C3_23 = H3*G2D3;
C3_31 = H3*G3D1; C3_32 = H3*G3D2; C3_33 = H3*G3D3;

C1 = [C1_11, C1_12, C1_13;
      C1_21, C1_22, C1_23;
      C1_31, C1_32, C1_33];
C1 = simplify(C1)

C2 = [C2_11, C2_12, C2_13;
      C2_21, C2_22, C2_23;
      C2_31, C2_32, C2_33];
C2 = simplify(C2)

C3 = [C3_11, C3_12, C3_13;
      C3_21, C3_22, C3_23;
      C3_31, C3_32, C3_33];
C3 = simplify(C3)



