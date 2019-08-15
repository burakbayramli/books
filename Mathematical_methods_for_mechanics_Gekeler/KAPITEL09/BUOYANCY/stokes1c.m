function [U,V,TT,P] = stokes1c(p,p1,t,t1,FU,FV,FT,RD,RCT,Parmeter);
% linear Stokes problem, TAYLOR HOOD elements
% One value for Pressure P must be (and is) specified

nu = Parmeter(1); lambda = Parmeter(2); epsilon = Parmeter(3);
g_beta = Parmeter(4); kappa = Parmeter(5); t_air = Parmeter(6);
[S,C,D,MM,BB,S_LIN] = taylor_hood(p,t,p1,t1);
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;

% Matrix A and Right side B ------------
NULL = sparse(N,N); 
A = [ nu*S, NULL, NULL,              -C;
     NULL , nu*S, -g_beta*MM,        -D;
     NULL,  NULL, lambda*S,     zeros(N,N1);
     C.'  , D.' , zeros(N1,N), epsilon*S_LIN];
B = [MM*FU; MM*FV; MM*FT; zeros(N1,1)];
%% -- Cauchy boundary conditions of temperature ---
if ~isempty(RCT)
   for I = 1:size(RCT,2)
      K       = RCT(1:3,I);
      [ME,BE] = fem_raqell(p(1,K),p(2,K));
      B(K)    = B(K) - kappa*t_air*BE; % - kappa*ME*T(K);
   end
end






% DIRICHLET boundary condition for U,V,P -----
J = RD(1,:); B = B - A(:,J)*RD(2,:).'; B(J) = RD(2,:).';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(size(A,1),1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
Z = A\B;
U = Z(1:N); V = Z(N+1:2*N); TT = Z(2*N+1:3*N); P = Z(3*N+1:3*N+N1);
