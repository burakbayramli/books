function [U,V,P] = stokes1b(p,p1,t,t1,FU,FV,RD,Parmeter);
% linear Stokes problem, TAYLOR HOOD elements
% NO value for Pressure P specified

nu = Parmeter(1); epsilon = Parmeter(2);
[S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;

BB = 100*BB; % Scaling, for pressure 
NULL = sparse(N,N); 
A = [nu*S, NULL,    -C;
     NULL, nu*S,    -D;
     C.' , D.' , epsilon*S_LIN];
% -- augmenting of A and B ----------      
AUX = [zeros(1,2*N),BB.'];  
A = [A; AUX]; A = [A,[AUX.';0]];   
B = [MM*FU; MM*FV; zeros(N1,1);0];
% DIRICHLET boundary conditions for U,V,P  -----
J = RD(1,:); B = B - A(:,J)*RD(2,:).'; B(J) = RD(2,:).';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(size(A,2),1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
Z = A\B;
U = Z(1:N); V = Z(N+1:2*N); P = Z(2*N+1:2*N+N1);
