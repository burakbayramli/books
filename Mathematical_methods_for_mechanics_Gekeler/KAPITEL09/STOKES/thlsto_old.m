function [SE,ME,CE,DE,SE_LIN,ecode,BE] = thlsto(X,Y)
% Supplies element matrices
% SE(6,6), ME(6,6), CE(6,3), DE(6,3), MP(3,3)
% for a TAYLOR-HOOD element in Stokes problems
% and stiffness matrix SE_LIN times h^2 of linear elements
% for stabilization after Sylvester, see Gresho II, p.625 ff. 

% INPUT:
% X,Y Coordinates of the three vertices
% ecode = 1 for wrong orientation

ecode = 0;
S1 = [3   1   0  -4   0   0;
      1   3   0  -4   0   0;
      0   0   0   0   0   0;
     -4  -4   0   8   0   0;
      0   0   0   0   8  -8;
      0   0   0   0  -8   8];
S2 = [6   1   1  -4   0  -4;
      1   0  -1  -4   4   0;
      1  -1   0   0   4  -4;
     -4  -4   0   8  -8   8;
      0   4   4  -8   8  -8;
     -4   0  -4   8  -8   8];
S3 = [3   0   1   0   0  -4;
      0   0   0   0   0   0;
      1   0   3   0   0  -4;
      0   0   0   8  -8   0;
      0   0   0  -8   8   0;
     -4   0  -4   0   0   8];

S4 = [6  -1  -1   0  -4   0;
     -1   6  -1   0   0  -4;
     -1  -1   6  -4   0   0;
      0   0  -4  32  16  16;
     -4   0   0  16  32  16;
      0  -4   0  16  16  32];
% for mass matrix of linear elements:
S5 = [2   1   1;  1   2   1;  1   1   2]; 
% for  ''int p dxdy = 0 '':
S6 = [1;1;1]; 

% For stiffness matrix of linear elements for pressure
SLIN1 = [1  -1   0; -1   1   0;  0   0   0];
SLIN2 = [2  -1  -1; -1   0   1; -1   1   0];
SLIN3 = [1   0  -1;  0   0   0; -1   0   1];

C1 = [...
 -1,  0,  0;
  0,  1,  0;
  0,  0,  0;
  1, -1,  0;
  1,  1,  2;
 -1, -1, -2]; % for DP/DX
C2 = [...
 -1,  0,  0;
  0,  0,  0;
  0,  0,  1;
 -1, -2, -1;
  1,  2,  1;
  1,  0, -1]; % for DP/DY
ecode = 0;
X21 = X(2) - X(1); X32 = X(3) - X(2); X31 = X(3) - X(1); X13 = - X31;
Y21 = Y(2) - Y(1); Y32 = X(3) - X(2); Y31 = Y(3) - Y(1); Y12 = - Y21;
XX = [X21, X32, X31]; YY = [Y21, Y32, Y31];
LL = max(abs(XX + (-1)*YY)); 
DET = X21*Y31 - X31*Y21;
if DET > 0
   A  =  (X31*X31 + Y31*Y31)/DET;
   B  = -(X31*X21 + Y31*Y21)/DET;
   C  =  (X21*X21 + Y21*Y21)/DET;
   SE =  (A*S1 + B*S2 + C*S3)/6; % stiffness matrix
   ME =  DET*S4/360;             % Mass matrix
   CE =  (Y31*C1 + Y12*C2)/6; 
   DE =  (X13*C1 + X21*C2)/6;
   % Stiffness matrix for linear elements:
   SE_LIN =  (A*SLIN1 + B*SLIN2 + C*SLIN3)/2; 
   SE_LIN = LL^2*SE_LIN;         % for Stabilization after Sylvester
 %  ML =  DET*S5/24  %mass matrix for linear elements not used 
   BE  = DET*S6/6; % load vector of linear elements
else ecode = 1; return    
end
