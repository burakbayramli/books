function Z = ellipt5(p,t,q,p1,p2,t1,q1,RD,RC,LASTEN,Parmeter);
% Elliptische Randwertprobleme 
% isoparametrische Dreieck- und Viereckelemente

RHO = Parmeter(1);
N = size(p,2); N1 = size(p1,2); N2 = size(p2,2);
N = N + N1 + N2;
A = sparse(N,N); B = zeros(N,1); C = sparse(N,N);
ecode       = 0;
% -- isoparametrische quadratische Dreieckelemente ---------
if ~isempty(t)
   for I = 1:size(t,2)
      J = t(1:3,I); K = t1(1:3,I); L = [J;K]; K = K - size(p,2);
      X = [p(1,J),p1(1,K)]; Y = [p(2,J),p1(2,K)];
      [SE,ME,BE,ecode] = fem_isodrq(X,Y);
      A(L,L)   = A(L,L) + SE + RHO*ME;
      C(L,L)   = C(L,L) + ME;
      B(L)     = B(L)   + BE;
      %  I
      %  ecode
   end
end
% -- isoparametrische quadratische Viereckelemente -------
clf
if ~isempty(q)
p_aux = [p,p1,p2];
   for I = 1:size(q,2)
      J = q(1:4,I); K = q1(1:4,I); L = [J;K]; %K = K-size(p,2)-size(p1,2);
      X = [p(1,J),p_aux(1,K)]; Y = [p(2,J),p_aux(2,K)];
      [SE,ME,BE,ecode] = fem_isopaq(X,Y);
      A(L,L)   = A(L,L) + SE + RHO*ME;
      C(L,L)   = C(L,L) + ME;
      B(L)     = B(L)   + BE;
   %  I
   %  ecode
   end
end
% --------------------------------------------------------
if length(LASTEN) == 1, B = B*LASTEN; else B = C*LASTEN'; end
% -- quadratische Randintegrale --------------------------
p_aux = [p,p1,p2];
for I = 1:size(RC,2)
   K = RC(1:3,I);;
   X = p_aux(1,K); Y = p_aux(2,K);
   [ME,BE] = fem_isoraq(X,Y);
   ALF = RC(4,I); BETA = RC(5,I);
   A(K,K) = A(K,K) + ALF*ME;
   B(K)   = B(K)   + BETA*BE;
end
% -- Dirichlet-Randbedingungen ----------------------------
J      = RD(1,:);
B      = B - A(:,J)*RD(2,:)';
B(J)   = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
AUX = zeros(N,1); AUX(RD(1,:)) = 1;
A = spdiags(diag(A)+AUX,0,A);
% -- LGS -------------------------------------------------
R = chol(A); Y =  R'\B; Z = R\Y;
