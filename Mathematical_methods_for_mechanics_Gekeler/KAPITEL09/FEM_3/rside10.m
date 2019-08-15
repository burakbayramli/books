function Y = rside10(T,W,NU,RSIDE)
% Computation of right side of Dgl

load daten10a p e t KK MM RDZ RDW NACHBAR
Z = ellipt1(p,t,RDZ,[],W);
% -- Randwerte fuer W -----
WBA  = wbound(p,e,e,t,RDW,W,Z,NACHBAR);
W(WBA(1,:)) = WBA(2,:)'; N = size(p,2);
% -- Convectiver Anteil -----------------------
B   = zeros(N,1);
for I = 1:size(t,2)
   K   = t(1:3,I);
   Z21 = Z(K(2))-Z(K(1)); Z31 = Z(K(3))-Z(K(1)); Z32 = Z(K(3))-Z(K(2));
   W_AUX  = (-Z32*W(K(1)) + Z31*W(K(2)) - Z21*W(K(3)))/6; % quadrat. Anteil
   B(K)   = B(K) + W_AUX;
end
RSW = - NU*KK*W - B + RSIDE;
Y = MM\RSW;
% -- Randwerte fuer Ableitung
Y(WBA(1,:)) = 0;
