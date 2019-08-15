function [p1,field1] = mesh04(p,field);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% eliminiert doppelte Knoten in der Knotenmatrix p
% und dem Feld der Knotennummern FIELD
% es werden die weiter rechts stehende doppelten Knoten
% eliminiert, notwendig fuer mesh06_t_q.m
% Schnellere Version ohne Ordnung siehe MESH01_t.M
L = size(p,2); p_aux = [[1:L];p]; [M,N] = size(field);
C      = field(:);
for I = 1:L
   for K = 1:I-1;
      if p_aux(2:3,I) == p_aux(2:3,K)
         J = find(C == I);
         C(J) = K;
         p_aux(1,I) = 0;
      end
   end
end
J = find(p_aux(1,:) ~= 0);
p_aux = p_aux(2:3,J);
for I = 1:size(p_aux,2);
   for K = 1:length(C)
      if p_aux(1:2,I) == p(1:2,C(K))
         C(K) = I;
      end
   end
end
field1 = reshape(C,[M,N]);
p1     = p_aux;
