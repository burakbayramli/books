function Z = stabwerk2(p,e,LAGER,LASTEN,E,F);
% Berechnet Verschiebungen in ebenen Stabwerken

N          = size(p,2);
N2         = 2*N;
A          = sparse(N2,N2);
for I = 1:size(e,2)
   [SE,ME] = stabelement1(p(1,e(:,I)),p(2,e(:,I)),E,F);
   L       = e(:,I) - 1;
   K       = [2*L(1)+[1:2], 2*L(2)+[1:2]];
   A(K,K)  = A(K,K) + SE;
 % I
end
B  = LASTEN(:)/2;  %!!!!!!
M2 = size(LAGER,2);
C = sparse(N2,M2);
for I = 1:M2
    K = LAGER(1,I);
    C(2*K-1:2*K,I) = LAGER(2:3,I);
end
RS = [B; zeros(M2,1)];
AA = [A, C;C' zeros(M2,M2)];
X  = AA\RS;
X  = X(1:N2);
Z  = zeros(N,2);
for I = 1:N
   Z(I,:) = X(2*(I-1)+[1:2])';
end
