function Z = ellipt_post(p,t,p1,t1,RD,RC,LASTEN)
% elliptische RWP
% gerade quadratische Dreieckelemente

RHO = 0;
N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
A = sparse(N,N); C = sparse(N,N); B = zeros(N,1);
% -- gerade quadratische Dreieckelemente ----
for I = 1:size(t,2)
   J = t(1:3,I); K = t1(:,I); L = [J;K];
   X = p(1,J); Y = p(2,J);
   [SE,ME,BE,ecode] = fem_drqell(X,Y);
   A(L,L) = A(L,L) + SE + RHO*ME;
   C(L,L) = C(L,L) + ME;
   B(L)   = B(L)   + BE;
end
% -------------------------------------------
if length(LASTEN) == 1, B = B*LASTEN; else B = LASTEN; end
% -- gerade quadratische Randintegrale ------
if ~isempty(RC)
for I = 1:size(RC,2)
   K       = RC([1,3],I);
   X       = p(1,K);
   Y       = p(2,K);
   [ME,BE] = fem_raqell(X,Y);
   L       = RC([1:3],I);
  %A(L,L)  = A(L,L) + RC(4,I)*ME;
  % B(L)    = B(L)  + RC(5,I)*BE;
end
end
% -- Dirichlet-Randbedingungen ---------------------------
J    = RD(1,:);
B    = B - A(:,J)*RD(2,:)';
B(J) = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(size(A,2),1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
% -- LGS -------------------------------------------------
R = chol(A); RAUX = R.'; Y = RAUX\B; Z = R\Y;
