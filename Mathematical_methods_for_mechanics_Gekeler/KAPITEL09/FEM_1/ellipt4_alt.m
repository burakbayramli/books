function Z = ellipt4a(N,KNOTEN,ELEMENTE1,ELEMENTE2,RD,RC,F,RHO)
% Elliptische RWP,  gerade kubische Elemente

A          = sparse(N,N);
B          = zeros(N,1);
ecode      = 0;
% -- gerade kubische Dreieckelemente ---------------------
for I = 1:size(ELEMENTE1,1)
   J        = zeros(3,1);
   K        = ELEMENTE1(I,1:3);
   for L = 1:3
      J(L)   = find(KNOTEN(:,1) == K(L));
   end
   X        = KNOTEN(J,2);
   Y        = KNOTEN(J,3);
   [SE,ME,BE,ecode] = drkell(X,Y);
   L        = ELEMENTE1(I,1:3);
   M        = [1  2  3];
   L        = L - 1;
   K        = [3*L(1)+M 3*L(2)+M 3*L(3)+M];
   A(K,K)   = A(K,K) + SE + RHO*ME;
   B(K)     = B(K) + BE;
end
% -- gerade kubische Parallelogrammelemente ------------------
for I = 1:size(ELEMENTE2,1)
   J        = zeros(3,1);
   M        = [1,2,4];
   K        = ELEMENTE2(I,M);
   for L = 1:3
      J(L)  = find(KNOTEN(:,1) == K(L));
   end
   X        = KNOTEN(J,2);
   Y        = KNOTEN(J,3);
   [SE,ME,BE,ecode] = pakell(X,Y);
   L        = ELEMENTE2(I,:);
   M        = [1  2  3];
   L        = L - 1;
   K        = [3*L(1)+M 3*L(2)+M 3*L(3)+M 3*L(4)+M];
   A(K,K)   = A(K,K) + SE + RHO*ME;
   B(K)     = B(K) + BE;
   %I
   %ecode
end
B           = F*B;
% -- gerade kubische Randintegrale
for I = 1:size(RC,1)
   J        = zeros(2,1);
   K        = RC(I,1:2);
   for L = 1:2
      J(L)  = find(KNOTEN(:,1) == K(L));
   end
   X        = KNOTEN(J,2);
   Y        = KNOTEN(J,3);
   [ME,BE]  = rakell(X,Y);
   L        = RC(I,1:2);
   ALF      = RC(I,3);
   BETA     = RC(I,4);
   M        = [1  2  3];
   L        = L - 1;
   K        = [3*L(1)+M 3*L(2)+M];
   A(K,K)   = A(K,K) + ALF*ME;
   B(K)     = B(K)   - BETA*BE;
   %I
   %ecode
end
% -- Dirichlet-Randbedingungen ---------------------------
for I = 1:size(RD,1)
   J        = RD(I,1);
   L        = J - 1;
   K        = 3*L + RD(I,2);
   B        = B + A(:,K)*RD(I,3);
end
for I = 1:size(RD,1)
   J        = RD(I,1);
   L        = J - 1;
   K        = 3*L + RD(I,2);
   B(K)     = - RD(I,3);
end
for I = 1:size(RD,1)
   J        = RD(I,1);
   L        = J - 1;
   K        = 3*L + RD(I,2);
   A(K,:)   = 0;
   A(:,K)   = 0;
   A(K,K)   = 1;
end
R           = chol(A);
Y           = - R'\B;
Z           = R\Y;
