function Z = ellipt_rt(p,p1,q,q1,RD,RC,LASTEN,Parmeter)
% elliptische RWP
% Rannacher-Turek-Elemente (RT-Elemente)
% Nichtkonforme Viereckelemente mit Knotenpunkten
% in den Seitenmitten

RHO = Parmeter(1);
N1 = size(p,2); N2 = size(p1,2); 
A = sparse(N2,N2); C = sparse(N2,N2); B = zeros(N2,1);
% -- RT-Elemente ----
for I = 1:size(q1,2)
   K = q(1:4,I); L = q1(1:4,I) - N1;
   [SE,ME,BE,ecode] = rt_ell(p(1,K),p(2,K));
   A(L,L) = A(L,L) + SE + RHO*ME;
   C(L,L) = C(L,L) + ME;
   B(L)   = B(L)   + BE;
end
% -------------------------------------------
if length(LASTEN) == 1, B = B*LASTEN; else B = C*LASTEN'; end
% -- gerade Randintegrale, Mittelpunktsregel ------
if ~isempty(RC)
   for I = 1:size(RC,2)
      K      = RC([1,2],I);
      X21    = p(1,K(2)) - p(1,K(1));
      Y21    = p(2,K(2)) - p(2,K(1));
      LL     = sqrt(X21*X21 + Y21*Y21);
      L      = RC(3,I)-size(p,2);
      ALF    = RC(4,I); BETA = RC(5,I); 
      A(L,L) = A(L,L) + ALF*LL;
      B(L)   = B(L)   + BETA*LL;
      %I
      %ecode
   end
end
% -- Dirichlet-Randbedingungen ---------------------------
J    = RD(1,:) - size(p,2);
B    = B - A(:,J)*RD(2,:)';
B(J) = RD(2,:)';
A(J,:) = 0; A(:,J) = 0;
for I = 1:size(RD,2), J = RD(1,I) - size(p,2); A(J,J)  = 1; end
% -- LGS -------------------------------------------------
R = chol(A); Y = R'\B; Z = R\Y;
