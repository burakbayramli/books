function Z = rahmen2(p,e,LAGER,LASTEN,PARMETER);
% Berechnet Verschiebungen in ALLGEMEINEN ebenen Balkenwerken

N = size(p,2); N3 = 3*N; A = sparse(N3,N3);
for I = 1:size(e,2)
   K       = e(1:2,I);
   H       = e(3,I);
   B       = e(4,I);
   X       = p(1,K);
   Z       = p(2,K);
   [SE,ME] = balken2(X,Z,H,B,PARMETER);
   L       = K - 1;
   M       = [1:3];
   K       = [3*L(1)+M, 3*L(2)+M];
   A(K,K)  = A(K,K) + SE;
 % I
end
B  = LASTEN(:)/2;
M2 = size(LAGER,2);
C  = sparse(N3,M2);
for I = 1:M2
    K = LAGER(1,I);
    C(3*K-2:3*K,I) = LAGER(2:4,I);
end
RS = [B; zeros(M2,1)];
AA = [A, C;C' zeros(M2,M2)];
X  = AA\RS;
X  = X(1:N3);
Z  = reshape(X,3,N);
