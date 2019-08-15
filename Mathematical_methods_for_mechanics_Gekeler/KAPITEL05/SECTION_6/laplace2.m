function [A,EW,U,V] = laplace2(n)
%function laplace2;
% n = 6;
% eigenvectors of Poisson problems in unit square 
% with zero boundary conditions for x = 0 and x = 1 only
% Let AUX = B_n+2 be the matrix B of dimension n+2
% and let C be obtained from AUX by setting
% AUX(1,2) = -2; AUX(n+2,n+1) = -2;
% Then C is NON-SYMMETRIC with same eigenvalues as AUX
% but right-eigenvectors can be calculated analytically 
% left-eigenvectors of C must be calculated numerically
% by MATLAB command NULL
% Numerical computation for simple EW 2*pi*pi 
clc, format short, format compact
e1 = ones(n,1); e2 = ones(n+2,1);
I1 = eye(n);    I2 = eye(n+2);
B  = spdiags([-e1, 2*e1, -e1], -1:1, n, n);
B = B*(n+1)^2;
C  = spdiags([-e2, 2*e2, -e2], -1:1, n+2, n+2);
C(1,2) = -2; C(n+2,n+1) = -2;
C = C*(n+1)^2;
A = (kron(B,I2) + kron(I1,C)); % numerical Laplace
A = sparse(A);
% -- Eigenvalues and eigenvectors --------------
p = 1;    
EW1  = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EVB1 = sin(pi*p*[1:n]/(n+1)).';
EVC1 = [1,cos(pi*p*[1:n+1]/(n+1))].';
U = kron(EVB1,EVC1); U = U/norm(U);
CTRANS   = C.';
RES_LEFT = CTRANS - EW1*eye(n+2);
V2= null(RES_LEFT); V2 = - V2;
V = kron(EVB1,V2); %V = V/norm(V);
V = V.'; EW = 2*EW1;
VU = V*U; V = V/VU; % normalization of V
check = 0;
if check == 1
   VU = V*U
   diff_eigenvalue1 = EW1 - pi*pi
   RESIDUUMB       = B*EVB1 - EW1*EVB1;
   NORM_RESIDUUM_B = max(abs(RESIDUUMB))
   RESIDUUMC       = C*EVC1 - EW1*EVC1;
   NORM_RESIDUUM_C = max(abs(RESIDUUMC))
   RESIDUUM_C_LEFT = V2.'*(C - EW1*eye(n+2));
   NORM_RESIDUUM_LEFT = max(abs(RESIDUUM_C_LEFT))
   
   RESIDUUM_A       = A*U - EW*U;
   NORM_RESIDUUM_A = max(abs(RESIDUUM_A))
   RESIDUUM_A_LEFT = V*(A - EW*eye(n*(n+2)));
   NORM_RESIDUUM_A_LEFT = max(abs(RESIDUUM_A_LEFT))
   
   %V2 = V2/norm(V2);
   EVC1 = EVC1;
   VU = V2.'*EVC1
   pause
end



