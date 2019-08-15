function [AU,BU,CV,DV] = triform1_aux(X,Y,U,V)
% Advection matrix for Taylor-Hood elements
% INPUT:
%     X,Y coordinates of corners of triangle
%     U,V velocity in x- and y-direction
% OUTPUT: (N column vector of shape functions in xi,eta,
%          therefore no transformation with design matrix!)
%     AU = int_T[(N_X'*U)*N*N'], BU = int_T[(N_Y'*U)*N*N']
%     CV = int_T[(N_X'*V)*N*N'], DV = int_T[(N_Y'*V)*N*N']
%     T triangle with corners (X(I),Y(I)), I = 1,2,3
% see also aasupplement.tex
 
X21  = X(2) - X(1); X31  = X(3) - X(1); X12 = - X21; X13 = - X31;
Y21  = Y(2) - Y(1); Y31  = Y(3) - Y(1); Y12 = - Y21; Y13 = - Y31;

M = [...
  6, -1, -1,  0, -4,  0;
 -1,  6, -1,  0,  0, -4;
 -1, -1,  6, -4,  0,  0;
  0,  0, -4, 32, 16, 16;
 -4,  0,  0, 16, 32, 16;
  0, -4,  0, 16, 16, 32]/360; 

P = [...
   6,  -4,   1,  -8, -12,  -4;
  -4,  30,  -4,  12,  12,  -4;
   1,  -4,   6, -12,  -8,  -4;
  -8,  12, -12,  96,  48,  32;
 -12,  12,  -8,  48,  96,  32;
  -4,  -4,  -4,  32,  32,  32]/(7*360); 
Q = [...
   6,   1,  -4,  -4, -12,  -8;
   1,   6,  -4,  -4,  -8, -12;
  -4,  -4,  30,  -4,  12,  12;
  -4,  -4,  -4,  32,  32,  32;
 -12,  -8,  12,  32,  96,  48;
  -8, -12,  12,  32,  48,  96]/(7*360); 
PX = [-3,-1, 0, 4,0, 0;  % Data of DPsi/Dxi
       4, 4, 0,-8,0, 0;
       4, 0, 0,-4,4,-4];

PY = [-3, 0,-1, 0,0, 4;  % Data of DPsi/Deta
       4, 0, 0,-4,4,-4;
       4, 0, 4, 0,0,-8];
       
F = PX*Y31 + PY*Y12; G = PX*X13 + PY*X21;

AU = F(1,:)*U*M + F(2,:)*U*P + F(3,:)*U*Q;
BU = G(1,:)*U*M + G(2,:)*U*P + G(3,:)*U*Q;
CV = F(1,:)*V*M + F(2,:)*V*P + F(3,:)*V*Q;
DV = G(1,:)*V*M + G(2,:)*V*P + G(3,:)*V*Q;