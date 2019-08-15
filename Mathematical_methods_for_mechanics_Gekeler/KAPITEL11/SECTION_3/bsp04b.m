function Y = bsp04b(T,X,Parmeter);
% Differentialsystem fuer Roboter ----------
% Massenmatrix (symmetrisch)
Z1 = X(1); GA1 = X(2); Y2 = X(3); BE2 = X(4); AL3 = X(5);
% Geometrische Daten -----------------------
C = 0.05; L = 0.50;
% Massen ----------------------
M1 = 250; M2 = 150; M3 = 100;
% Traegheitsmomente ---------------
IX1 = 90; IX2 = 13; IX3 = 4;
IY1 = 10; IY2 = 0.75; IY3 = 1;
IZ1 = 90; IZ2 = 13; IZ3 = 4.3;
% -----------------------------
MM      = zeros(5,5);
MM(1,1) =  M1 + M2 + M3;
MM(2,2) =   M3*C^2*cos(AL3)^2*cos(BE2)^2 + IY3*sin(AL3)^2*cos(BE2)^2 ...
          + IZ3*cos(AL3)^2*cos(BE2)^2 + M3*C^2*sin(BE2)^2 ...
          + IX2*sin(BE2)^2 + IZ2*cos(BE2)^2 + IX3*sin(BE2)^2 ...
          + 2*M3*C*Y2*cos(AL3) + 2*M3*C*L*cos(AL3) ...
          + M2*Y2^2 + M3*Y2^2 + 2*M3*L*Y2 + M3*L^2 + IZ1;
MM(3,2) =   M3*C*sin(AL3)*sin(BE2);
MM(3,3) =   M2 + M3;
MM(4,1) = - M3*C*sin(AL3)*sin(BE2);
MM(4,2) = - M3*C^2*sin(AL3)*cos(AL3)*cos(BE2) ...
          - M3*C*Y2*sin(AL3)*cos(BE2)-M3*C*L*sin(AL3)*cos(BE2) ...
          + IY3*sin(AL3)*cos(AL3)*cos(BE2) - IZ3*sin(AL3)*cos(AL3)*cos(BE2);
MM(4,4) =   M3*C^2*sin(AL3)^2 + IY3*cos(AL3)^2 + IZ3*sin(AL3)^2 + IY2;
MM(5,1) =   M3*C*cos(AL3)*cos(BE2);
MM(5,2) = - M3*C*Y2*sin(BE2)*cos(AL3) - M3*C*L*sin(BE2)*cos(AL3) ...
          - M3*C^2*sin(BE2) - IX3*sin(BE2);
MM(5,3) = - M3*C*sin(AL3);
MM(5,5) =   M3*C^2 + IX3;
MM(1,3) =   MM(3,1);
MM(2,3) =   MM(3,2);
MM(1,4) =   MM(4,1);
MM(2,4) =   MM(4,2);
MM(1,5) =   MM(5,1);
MM(2,5) =   MM(5,2);
Y       =   [eye(5), zeros(5,5); zeros(5,5), MM];
Y       =   sparse(Y);
%DETMM = det(MM)
