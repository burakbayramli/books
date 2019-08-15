function Z = ellipt(p,t,RD,RC,LASTEN,Parmeter);
% Elliptic BVP, linear triangular elements
ecode = 0;
RHO = Parmeter(1);
N = size(p,2); A = sparse(N,N); C = A; B = zeros(N,1);
for I = 1:size(t,2)
   K = t(1:3,I);
   [SE,ME,BE,ecode] = couranta(p(1,K),p(2,K));
   A(K,K)  = A(K,K) + SE + RHO*ME; % stiffness matrix
   C(K,K)  = C(K,K) + ME;          % mass matrix
   B(K)    = B(K)   + BE;          % right side 
end
if length(LASTEN) == 1, B = B*LASTEN; else B = C*LASTEN; end
% -- linear boundary integrals --------
for I = 1:size(RC,2)
  K = RC(1:2,I);
  [ME,BE] = courantb(p(1,K),p(2,K));
  ALF = RC(3,I); BETA = RC(4,I);
  A(K,K) = A(K,K) + ALF*ME;
  B(K)   = B(K)   + BETA*BE;
 % I
 % ecode
end
% -- Dirichlet boundary conditions, modify matrix! ---
J = RD(1,:); B = B - A(:,J)*RD(2,:)'; B(J) = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(N,1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
R = chol(A); Y = R'\B; Z = R\Y; % Solving linear system
