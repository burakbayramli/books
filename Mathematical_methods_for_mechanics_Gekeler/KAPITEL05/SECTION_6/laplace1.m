function [A,EW,UU] = laplace1(n,NU)
%function laplace1, n = 24; NU = 3;
% eigenvectors of Poisson problems in unit square 
% with zero boundary conditions everywhere
% Numerical computation for simple EW 2*pi*pi 
clc, format short, format compact
e = ones(n,1); I = eye(n);
B = spdiags([-e, 2*e, -e], -1:1, n, n);
A = (kron(B,I) + kron(I,B))*(n+1)^2;
A = sparse(A);
switch NU
case 1
   EWA = 4*(n+1)^2*( sin(1*pi/(2*(n+1)))  )^2;
   EW = 2*EWA;
   AUX1 = sin(pi*[1:n]/(n+1)).';
   U1 = kron(AUX1,AUX1); U1 = U1/norm(U1);
   %RES = A*U1 - EW*U1; ResNorm = max(abs(RES))
   UU = U1;
case 2
   EWA = 4*(n+1)^2*( sin(1*pi/(2*(n+1)))  )^2;
   EWB = 4*(n+1)^2*( sin(2*pi/(2*(n+1)))  )^2;
   EW = EWA + EWB;
   AUX1 = sin(pi*[1:n]/(n+1)).';
   AUX2 = sin(2*pi*[1:n]/(n+1)).';
   U1 = kron(AUX1,AUX2);  U1 = U1/norm(U1);
   U2 = kron(AUX2,AUX1);  U1 = U1/norm(U1);
   UU = [U1,U2];
   %RES1 = A*U1 - EW*U1; ResNorm1 = max(abs(RES1))
   %RES2 = A*U2 - EW*U2; ResNorm2 = max(abs(RES2))
case 3   
   EWA = 4*(n+1)^2*( sin(1*pi/(2*(n+1)))  )^2;
   EWB = 4*(n+1)^2*( sin(7*pi/(2*(n+1)))  )^2;
   EWC = 4*(n+1)^2*( sin(5*pi/(2*(n+1)))  )^2;
   EW = EWA + EWB
   % Note that EW1A + EW3A ~= 2*EW3B (equal only in limit)
   % therefore some discrepancies in numerical approximation
   AUX1 = sin(pi*[1:n]/(n+1)).';
   AUX3 = sin(5*pi*[1:n]/(n+1)).';
   AUX4 = sin(7*pi*[1:n]/(n+1)).';
   U1 = kron(AUX1,AUX4);  U1 = U1/norm(U1);
   U2 = kron(AUX4,AUX1);  U2 = U2/norm(U2);
   U3 = kron(AUX3,AUX3);  U3 = U3/norm(U3);
   UU = [U1,U2,U3];
   %RES1 = A*U1 - EW*U1; ResNorm1 = max(abs(RES1))
   %RES2 = A*U2 - EW*U2; ResNorm2 = max(abs(RES2))
   %RES3 = A*U3 - 2*EW3B*U3; ResNorm3 = max(abs(RES3))
end

