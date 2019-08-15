function [U,V,P] = stokes3(p,p1,t,t1,RDU,RDV,RDP,FU,FV,Parmeter,CC)
% Stokes problem, TAYLOR-HOOD elements with convection term
% Declare one value for Pressure P

nu = Parmeter(1); epsilon = Parmeter(2);
[S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;

% --------------------------------------
A = [nu*S+CC(1:N,1:N), CC(1:N,N+1:2*N)         , -C;
     CC(N+1:2*N,1:N) , nu*S+CC(N+1:2*N,N+1:2*N), -D;
     C.'             , D.'     , epsilon*S_LIN];
B = [MM*FU; MM*FV; zeros(N1,1)];
% DIRICHLET boundary condition  -----
RD = [RDU(1,:), RDV(1,:) + N,RDP(1)+2*N;
      RDU(2,:), RDV(2,:)    ,RDP(2)];
J = RD(1,:); B = B - A(:,J)*RD(2,:)'; B(J)= RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(size(A,1),1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
Z = A\B;
% Full output:
U = Z(1:N); V = Z(N+1:2*N); P = Z(2*N+1:2*N+N1);
