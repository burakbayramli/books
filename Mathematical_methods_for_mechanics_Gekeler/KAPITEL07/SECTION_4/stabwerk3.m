function Z = stabwerk3(p,e,LAGER,LASTEN,PARMETER);
% Berechnet VERSCHIEBUNGEN
% in raeumlichen Stabwerken

E = PARMETER(1);
F = PARMETER(2);
N          = size(p,2);
N3         = 3*N;
A          = sparse(N3,N3);
for I = 1:size(e,2)
   K       = e(:,I);
   [SE,ME] = stabelement2(p(1,K),p(2,K),p(3,K),E,F);
   L       = K - 1;
   K       = [3*L(1)+[1:3], 3*L(2)+[1:3]];
   A(K,K)  = A(K,K) + SE;
 % I
end
% -- LASTEN ----------------------------------------------
B          = LASTEN(:)/2;
% -- LAGER, -----------------------------------------------
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
Z = 2*Z';  % Faktor 2 fehlt irgendwo, vgl. Schwarz
