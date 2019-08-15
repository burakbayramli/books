function [A,EW,U,V] = laplace3(n)
%function laplace3;
% eigenvectors of Poisson problems in unit square 
% with zero boundary conditions for x = 0 and x = 1 only
% Let AUX = B_n+2 be the matrix B of dimension n+2
% and let C be obtained from AUX by setting
% AUX(1,2) = -2; AUX(n+2,n+1) = -2;
% Then C is NON-SYMMETRIC with same eigenvalues as AUX
% but right-eigenvectors can be calculated analytically 
% left-eigenvectors of C must be calculated numerically
% by MATLAB command NULL
% Numerical computation for double EW 5*pi*pi 

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
p = 1;    
EW1  = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EVB1 = sin(pi*p*[1:n]/(n+1)).';
EVC1 = [1,cos(pi*p*[1:n+1]/(n+1))].';
p = 2;    
EW2  = 4*(n+1)^2*( sin(pi*p/(2*(n+1)))  )^2;
EVB2 = sin(pi*p*[1:n]/(n+1)).';
EVC2 = [1,cos(pi*p*[1:n+1]/(n+1))].';

U1 = kron(EVB1,EVC2); U1 = U1/norm(U1);
U2 = kron(EVB2,EVC1); U2 = U2/norm(U2);
U = [U1,U2];
CTRANS   = C.';
RES_LEFT = CTRANS - EW1*eye(n+2);
VAUX1 = null(RES_LEFT); 
RES_LEFT = CTRANS - EW2*eye(n+2);
VAUX2 = null(RES_LEFT); %VAUX2 = VAUX2/norm(VAUX2);

V1 = kron(EVB1,VAUX2); %V1 = V1/norm(V1);
V2 = kron(EVB2,VAUX1); %V2 = V2/norm(V2);
V = [V1,-V2];
V = V.'; VU = V*U;
V = inv(VU)*V; % normalization of V
EW = EW1 + EW2;
check = 0;
if check == 1
   VU = V*U
   diff_eigenvalue1 = EW1 - pi*pi
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
   VAUX1 = VAUX1/norm(VAUX1);
   VAUX2 = VAUX2/norm(VAUX2);
   EVC1 = EVC1/norm(EVC1);
   EVC2 = EVC2/norm(EVC2);

   VAUX = [VAUX1,VAUX2]; UAUX = [EVC1,EVC2];
   VU = VAUX.'*UAUX
end



