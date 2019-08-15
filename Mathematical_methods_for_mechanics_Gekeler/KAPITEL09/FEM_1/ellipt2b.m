function Z = ellipt2b(p,t,RD,RC,LASTEN,Parmeter);
% Elliptische RWP, bilineare Viereckelemente

RHO = Parmeter(1); ecode = 0; N = size(p,2);
A = sparse(N,N); C = sparse(N,N); B = zeros(N,1);
% -- bilineare Viereckelemente -------------------------
for I = 1:size(t,2)
   K       = t(:,I);
   X = p(1,K); Y = p(2,K);
   [SE,ME,BE,ecode] = fem_isobil(X,Y);
   A(K,K) = A(K,K) + SE + RHO*ME;
   C(K,K) = C(K,K) + ME;
   B(K)   = B(K) + BE;
  % I
  % ecode
end
if length(LASTEN) == 1, B = LASTEN*B; else, B = C*LASTEN'; end
% -- CAUCHY-Randbedingungen --------------
for I = 1:size(RC,2)
  K = RC(1:2,I);
  X = p(1,K); Y = p(2,K);
  [ME,BE]  = ralell(X,Y);
  ALF = RC(3,I); BETA = RC(4,I);
  A(K,K) = A(K,K) + ALF*ME;
  B(K)   = B(K)   + BETA*BE;
 % I
 % ecode
end
% -- DIRICHLET-Randbedingungen ------------
J    = RD(1,:);
B    = B - A(:,J)*RD(2,:)';
B(J) = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(N,1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
% -- Loesung des Gleichungssystems ----------------------
R = chol(A); Y = - R'\B; Z = R\Y;
% -------------------------------------------------------
