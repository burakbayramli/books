function Z = ellipt1(p,t,RD,RC,F);
% Elliptische RWP, gerade lineare Dreieckselemente
% andere Behandlung der rechten Seite
ecode      = 0;
N = size(p,2);
A = sparse(N,N); C = sparse(N,N); B = zeros(N,1);
% -- lineare Dreieckelemente ---------------------
for I = 1:size(t,2)
   K       = t(1:3,I);
   [SE,ME,BE,ecode] = drlell(p(1,K),p(2,K));
   A(K,K)  = A(K,K) + SE;  % + RHO*ME, RHO = 0;
   C(K,K)  = C(K,K) + ME;
   B(K)    = B(K) + BE;
end
if length(F) == 1, B = B*F; else B = C*F; end
% -- lineare Randintegrale ------------------------
if ~isempty(RC)
   for I = 1:size(RC,1)
      J = zeros(2,1); K = RC(I,1:2);
      [ME,BE] = ralell(p(1,K),p(2,K));
      ALF = RC(I,3); BETA = RC(I,4);
      A(K,K) = A(K,K) + ALF*ME;
      B(K)   = B(K)   + BETA*BE;
   end
end
% -- Dirichlet-Randbedingungen -------------------
% -- Matrix abaendern ! ------------------
J      = RD(1,:);
B      = B - A(:,J)*RD(2,:)';
B(J)   = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(N,1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
% -- Loesung des Gleichungssystems ----------------
R = chol(A); Y = R'\B; Z = R\Y;
%LOESUNG    = [Z KNOTEN(:,2:3)]
