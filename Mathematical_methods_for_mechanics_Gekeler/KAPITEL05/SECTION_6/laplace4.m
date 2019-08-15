function [A,EW,U,V] = laplace4(n)
%function laplace4;
% eigenvectors of Poisson problems in unit square 
% with zero boundary conditions for x = 0 and x = 1 only
% Let AUX = B_n+2 be the matrix B of dimension n+2
% and let C be obtained from AUX by setting
% AUX(1,2) = -2; AUX(n+2,n+1) = -2;
% Then C is NON-SYMMETRIC with same eigenvalues as AUX
% but right-eigenvectors can be calculated analytically 
% left-eigenvectors of C must be calculated numerically
% by MATLAB command NULL
% Numerical computation for trifold EW 50*pi*pi 

%n = 24;
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
P = [1,7,5];
%P = [3,4,5];

p = P(1);    
EW1  = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EVB1 = sin(pi*p*[1:n]/(n+1)).';
EVC1 = [1,cos(pi*p*[1:n+1]/(n+1))].';
p = P(2);    
EW2  = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EVB2 = sin(pi*p*[1:n]/(n+1)).';
EVC2 = [1,cos(pi*p*[1:n+1]/(n+1))].';
p = P(3);    
EW3  = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EVB3 = sin(pi*p*[1:n]/(n+1)).';
EVC3 = [1,cos(pi*p*[1:n+1]/(n+1))].';

U1 = kron(EVB1,EVC2); U1 = U1/norm(U1);
U2 = kron(EVB2,EVC1); U2 = U2/norm(U2);
U3 = kron(EVB3,EVC3); U3 = U3/norm(U3);
U = [U1,U2,U3];
CTRANS   = C.';
RES_LEFT = CTRANS - EW1*eye(n+2);
VAUX1 = null(RES_LEFT);
RES_LEFT = CTRANS - EW2*eye(n+2);
VAUX2 = null(RES_LEFT);
RES_LEFT = CTRANS - EW3*eye(n+2);
VAUX3 = null(RES_LEFT);

V1 = kron(EVB1,VAUX2); V1 = V1/norm(V1);
V2 = kron(EVB2,VAUX1); V2 = V2/norm(V2);
V3 = kron(EVB3,VAUX3); V2 = V2/norm(V2);

V = [V1,V2,V3]; V = V.'; 
VU = V*U;
V = inv(VU)*V; % normalization of V
EW = EW1 + EW2;
EWA = 2*EW3;
% ATTENTION: EW NOTEQUAL EWA, only in the limit
% therefore some discrepancies
check = 0;
if check == 1
   VU = V*U
   diff_eigenvalue1 = EW1 - P(1)^2*pi*pi
   diff_eigenvalue2 = EW2 - P(2)^2*pi*pi
   diff_eigenvalue3 = EW3 -  P(3)^2*pi*pi

   RESIDUUMB1       = B*EVB1 - EW1*EVB1;
   NORM_RESIDUUM_B1 = max(abs(RESIDUUMB1))
   RESIDUUMB2       = B*EVB2 - EW2*EVB2;
   NORM_RESIDUUM_B2 = max(abs(RESIDUUMB2))

   RESIDUUMC1       = C*EVC1 - EW1*EVC1;
   NORM_RESIDUUM_C1 = max(abs(RESIDUUMC1))
   RESIDUUMC2       = C*EVC2 - EW2*EVC2;
   NORM_RESIDUUM_C2 = max(abs(RESIDUUMC2))

   RESIDUUM_C_LEFT1    = VAUX1.'*(C - EW1*eye(n+2));
   NORM_RESIDUUM_LEFT1 = max(abs(RESIDUUM_C_LEFT1))
   RESIDUUM_C_LEFT2    = VAUX2.'*(C - EW2*eye(n+2));
   NORM_RESIDUUM_LEFT2 = max(abs(RESIDUUM_C_LEFT2))
   RESIDUUM_C_LEFT3    = VAUX3.'*(C - EW3*eye(n+2));
   NORM_RESIDUUM_LEFT3 = max(abs(RESIDUUM_C_LEFT3))

end



