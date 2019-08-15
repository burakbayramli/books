function [RDKN,t_rand,t_innen] = mesh23(p,e,t,segnr1);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% berechnet die Nummer der Dreieckselemente, die zum Rand gehoeren
% und den zugehoerigen Knotenvektor
% einfach zusammenhaengendes Gebiet
E = [];
for I = 1:length(segnr1)
   J     = find(e(5,:) == segnr1(I));
   E = [E,e(1,J)];
end
N = size(p,2); M = size(t,2); KK = zeros(1,M);
for I = 1:length(E)
   for J = 1:M
      if any(t(:,J) == E(I)) & KK(J) == 0
         KK(J) = J;
      end
   end
end
L1 = find(KK ~= 0); RDEL  = KK(L1);
t_rand  = t(:,RDEL);
L2      = find(KK == 0);
t_innen = t(:,L2);
% -- KNOTEN -----------------------------------------------
RDKN   = zeros(1,N);
for I = 1:N
   for J = 1:length(RDEL)
      for K = 1:3
         if t(K,L1(J)) == I & RDKN(I) == 0
            RDKN(I) = I;
         end
      end
   end
end
L = find(RDKN ~= 0); RDKN = RDKN(L);
