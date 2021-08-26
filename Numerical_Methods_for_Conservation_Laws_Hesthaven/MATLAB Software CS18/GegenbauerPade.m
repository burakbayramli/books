function [uPade] = GegenbauerPade(b,r,lambda,N,M,L);
% function [uPade] = GegenbauerPade(b,x,N,M,L);
% Purpose: Express Gegenbauer expansion as Pade form to suppress
% Runge phenomenon in diagonal limit
NQ = 2*N; [xG,wG] = GegenbauerGQ(lambda,NQ);

% Evaluate Gegenbauer polynomials at nodes to order 2*N
 Cmat = zeros(NQ+1,N+1);
 for m=0:N
    Cmat(:,m+1) = GegenbauerP(xG,lambda,m);
 end
 
 % Evaluate function at quadrature points
 u = Cmat*b;
 
 % Set up coefficient matrix
 H = Cmat'*diag(u.*wG)*Cmat;
 
 % Compute coefficient for q
 Hq = H(M+2:N+1,1:L+1);
 Z = null(Hq); q = Z(:,end); q = q/min(q);
 
 % Compute coefficients for p
 p = H(1:M+1,1:L+1)*q;
 
 % Evaluate Pade form at r
 rlen = length(r); Cmat = zeros(rlen,N+1);
 for m=0:N
    Cmat(:,m+1) = GegenbauerP(r,lambda,m);
 end
 
 pG = Cmat(:,1:M+1)*p; qG = Cmat(:,1:L+1)*q;
 uPade = pG./qG;
 return
 