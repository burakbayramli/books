function [LOESUNG,SIGD,SIG1,SIG2,PHI]...
       = scheibe3(p,t,RDU,RDV,LASTENU,LASTENV,Parmeter);
% disc computation with stress computation
% linear cubic triangles with condensation

E = Parmeter(1); NU = Parmeter(2); H = Parmeter(3);

N = size(p,2); N6 = 6*N;
% -- Dreieckelemente --------------------------------------
A          = sparse(N6,N6);
for I = 1:size(t,2)
   K  = t(:,I);
   XK = p(1,K); YK = p(2,K);
   [SE,ME,ecode] = fem_drksch(XK,YK,NU);
   L       = t(:,I) - 1;
   M       = [1:6];
   K       = [6*L(1)+M,6*L(2)+M,6*L(3)+M];
   A(K,K)  = A(K,K) + SE;
   I_ecode = [I,ecode];
end
FAKTOR = (1 - NU*NU)/(E*H);
B          = zeros(N6,1);
% -- Loads for U --------------
for I = 1:size(LASTENU,2)
   L       = LASTENU(1,I);
   M       = 6*(L-1) + 1;
   B(M)    = LASTENU(2,I)*FAKTOR;
end
% -- Loads for V ----------------
for I = 1:size(LASTENV,2)
   L       = LASTENV(1,I);
   M       = 6*(L-1) + 2;
   B(M)    = LASTENV(2,I)*FAKTOR;
end
% -- Dirichlet boundary condition for U --
if ~isempty(RDU)
   NN = size(RDU,2);
   for I = 1:NN
      L = RDU(1,I); M = 6*(L-1) + 1;
      B = B - RDU(2,I)*A(:,M);
   end
   for I = 1:NN
      L = RDU(1,I); M = 6*(L-1) + 1;
      B(M) = RDU(2,I); A(M,:) = 0; A(:,M) = 0; A(M,M)  = 1;
   end
end
%fuer v
if ~isempty(RDV)
   NN         = size(RDV,2);
   for I = 1:NN
      L = RDV(1,I); M = 6*(L-1) + 2;
      B = B - RDV(2,I)*A(:,M);
   end
   for I = 1:NN
      L  = RDV(1,I); M = 6*(L-1) + 2;
      B(M) = RDV(2,I); A(M,:) = 0; A(:,M)  = 0; A(M,M)  = 1;
   end
end
% -- LGS -------------------------------------------------
  %R = chol(A); Y = R'\B; Z = R\Y;
Z = A\B;
LOESUNG    = zeros(6,N);
for I = 1:N
   LOESUNG(:,I) = Z(6*(I-1)+[1:6]);
end
