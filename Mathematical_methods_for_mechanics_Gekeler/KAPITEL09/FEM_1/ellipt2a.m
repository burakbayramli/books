function Z = ellipt2a(p,t,RD,RC,LASTEN,Parmeter);
% Elliptische RWP, lineare Parallelogrammelemente
% Alternative waehlen, wenn F skalar
RHO = Parmeter(1); ecode = 0; N = size(p,2);
C = sparse(N,N); A = sparse(N,N); B = zeros(N,1);
% -- lineare Parallelogrammelemente --------------------
for I = 1:size(t,2)
   K = t([1,2,4],I);
   X = p(1,K); Y = p(2,K);
   [SE,ME,BE,ecode] = fem_bilin(X,Y);
   K = t(1:4,I);
   A(K,K) = A(K,K) + SE + RHO*ME;
   C(K,K) = C(K,K) + ME;
   B(K)   = B(K) + BE;
   %I_ecode = [I,ecode];
end
% -- Rechte Seite ---------------------------
if length(LASTEN) == 1, B = LASTEN*B; else, B = C*LASTEN'; end
% -- CAUCHY-Randbedingungen ---------------------------
if ~isempty(RC)
for I = 1:size(RC,2)
  K = RC(1:2,I);
  X = p(1,K); Y = p(2,K);
  [ME,BE] = fem_ralell(X,Y);
  ALF = RC(3,I); BETA = RC(4,I);
  A(K,K) = A(K,K) + ALF*ME;
  B(K)   = B(K)   + BETA*BE;
 % I
 % ecode
end
end
% -- Dirichlet-Randbedingungen --------------------------
J    = RD(1,:);
B    = B - A(:,J)*RD(2,:)';
B(J) = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(N,1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
% -- Loesung des Gleichungssystems ----------------------
%R = chol(A); Y = R'\B; Z = R\Y;
Z = A\B;
% ------------------------------------------------------
