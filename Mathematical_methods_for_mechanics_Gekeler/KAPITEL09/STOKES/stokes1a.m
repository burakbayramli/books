function [U,V,P] = stokes1a(p,p1,t,t1,FU,FV,RD,Parmeter);
% linear Stokes problem, TAYLOR HOOD elements
% One value for Pressure P must be (and is) specified

nu = Parmeter(1); epsilon = Parmeter(2);
[S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;

% Matrix A and Right side B ------------
NULL = sparse(N,N); 
A = [ nu*S, NULL,    -C;
     NULL , nu*S,    -D;
     C.'  , D.' , epsilon*S_LIN];
B = [MM*FU; MM*FV; zeros(N1,1)];
% DIRICHLET boundary condition for U,V,P -----
J = RD(1,:); B = B - A(:,J)*RD(2,:).'; B(J) = RD(2,:).';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(size(A,1),1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
Z = A\B;
U = Z(1:N); V = Z(N+1:2*N); P = Z(2*N+1:2*N+N1);
