function Z = ellipt3(p,t,q,p1,p2,t1,q1,RD,RC,LASTEN,Parmeter)
% elliptische RWP
% gerade quadratische Dreieck- und Parallelogrammelemente
%clf, hold on
%XX = p(1,:); YY = p(2,:); ZZ = zeros(1,length(XX));
%trimesh(t',XX,YY,ZZ), hold on
%mesh36(p,q,'b'), hold on

RHO = Parmeter(1);
N = size(p,2); N1 = size(p1,2); N2 = size(p2,2);
N = N + N1 + N2;
A = sparse(N,N); C = sparse(N,N); B = zeros(N,1);
% -- gerade quadratische Dreieckelemente ----
if ~isempty(t)
   for I = 1:size(t,2)
      J = t(1:3,I); K = t1(:,I); L = [J;K];
      X = p(1,J); Y = p(2,J);
      [SE,ME,BE,ecode] = fem_drqell(X,Y);
      A(L,L) = A(L,L) + SE + RHO*ME;
      C(L,L) = C(L,L) + ME;
      B(L)   = B(L)   + BE;
   end
end
%-- geradlinige Parallelogrammelemente -----
if ~isempty(q)
   M = [1, 2, 4];
   for I = 1:size(q,2)
      J = q(M,I); K = q1(:,I); L = [q(1:4,I);K];
      X = p(1,J); Y = p(2,J);
      [SE,ME,BE,ecode] = fem_paqell(X,Y);
      A(L,L) = A(L,L) + SE + RHO*ME;
      C(L,L) = C(L,L) + ME;
      B(L)   = B(L)   + BE;
   end
end
% -------------------------------------------
if length(LASTEN) == 1, B = B*LASTEN; else B = C*LASTEN'; end
% -- gerade quadratische Randintegrale ------
for I = 1:size(RC,2)
   K = RC([1,3],I);
   X = p(1,K); Y = p(2,K);
   [ME,BE] = fem_raqell(X,Y);
   L = RC([1:3],I);
   ALF  = RC(4,I);
   BETA = RC(5,I);
   A(L,L) = A(L,L) + ALF*ME;
   B(L)   = B(L)   + BETA*BE;
   %I
   %ecode
end
% -- Dirichlet-Randbedingungen ---------------------------
J    = RD(1,:);
B    = B - A(:,J)*RD(2,:)';
B(J) = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(N,1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
% -- LGS -------------------------------------------------
R = chol(A); Y = R'\B; Z = R\Y;
