function Z = rahmen2(p,e,LAGER,LASTEN,PARMETER);
% Berechnet VERSCHIEBUNGEN in raeumlichen Balkenwerken

N          = size(p,2);
N6         = 6*N;
A          = sparse(N6,N6);
for I = 1:size(e,2)
   K       = e(1:2,I);
   H       = e(3,I);
   B       = e(4,I);
   X       = p(1,K);
   Y       = p(2,K);
   Z       = p(3,K);
   [SE,ME] = balken2(X,Y,Z,H,B,PARMETER);
   L       = K - 1;
   M       = [1:6];
   K       = [6*L(1)+M, 6*L(2)+M];
   A(K,K)  = A(K,K) + SE;
 % I
end
B  = LASTEN(:);
M2 = size(LAGER,2);
C  = sparse(N6,M2);
for I = 1:M2
    K = LAGER(1,I);
    C(6*K-5:6*K,I) = LAGER(2:7,I);
end
RS = [B; zeros(M2,1)];
AA = [A, C;C' zeros(M2,M2)];
X  = AA\RS;
X  = X(1:N6);
Z  = reshape(X,6,N);
