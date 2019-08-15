function Z = ellipt4(p,t,q,RD,RC,LASTEN,Parmeter)
% Elliptische RWP,
% kubische Dreieck- und Parallelogrammelemente

RHO = Parmeter(1);
N = size(p,2); N3 = 3*N;
A = sparse(N3,N3); B = zeros(N3,1); C = sparse(N3,N3);
ecode      = 0;
% -- gerade kubische Dreieckelemente -------
if ~isempty(t)
   for I = 1:size(t,2)
      J = t(1:3,I);
      X = p(1,J); Y = p(2,J);
      [SE,ME,BE,ecode] = fem_drkell(X,Y);
      L = J - 1; M = [1:3];
      K = [3*L(1)+M, 3*L(2)+M, 3*L(3)+M];
      A(K,K) = A(K,K) + SE + RHO*ME;
      C(K,K) = C(K,K) + ME;
      B(K)   = B(K) + BE;
   end
end
% -- gerade kubische Parallelogrammelemente --
if ~isempty(q)
   for I = 1:size(q,2)
      J = q([1,2,4],I);
      X = p(1,J); Y = p(2,J);
      [SE,ME,BE,ecode] = fem_pakell(X,Y);
      L = q(1:4,I)-1; M = [1:3];
      K = [3*L(1)+M, 3*L(2)+M, 3*L(3)+M, 3*L(4)+M];
      A(K,K) = A(K,K) + SE + RHO*ME;
      C(K,K) = C(K,K) + ME;
      B(K)   = B(K) + BE;
      %I
      %ecode
   end
end
% -------------------------------------------
if length(LASTEN) == 1, B = B*LASTEN;
else B = C*LASTEN'; end
% -----------------------------------------
% -- gerade kubische Randintegrale
if ~isempty(RC)
   for I = 1:size(RC,2)
      J = RC(1:2,I);
      X = p(1,J); Y = p(2,J);
      [ME,BE] = fem_rakell(X,Y);
      ALF = RC(3,I); BETA = RC(4,I);
      M   = [1, 2, 3]; L  = J - 1;
      K   = [3*L(1)+M, 3*L(2)+M];
      A(K,K) = A(K,K) + ALF*ME;
      B(K)   = B(K)   + BETA*BE;
      %I
      %ecode
   end
end
% -- Dirichlet-Randbedingungen ---------------
for I = 1:size(RD,2)
   J = RD(1,I); L = J - 1; K = 3*L + 1;
   B    = B - A(:,K)*RD(2,I);
   B(K) = RD(2,I);
end
for I = 1:size(RD,2)
   J = RD(1,I); L = J - 1; K = 3*L + 1;
   A(K,:) = 0; A(:,K) = 0; A(K,K) = 1;
end
R = chol(A); Y = R'\B; Z = R\Y;
