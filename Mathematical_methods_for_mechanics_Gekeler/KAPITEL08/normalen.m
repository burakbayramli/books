function w_n = normalen(p,e,w)
% Berechnung der Normalableitungen von w am Rand

global t
M = size(t,2); N = size(e,2); P = size(p,2);
AUX = zeros(1,N); w_n  = zeros(1,N); NN  = zeros(2,N);
NUMMER = t(1:3,:);
for I = 1:N
   for K = 1:M
      M1 = find(NUMMER(:,K) == e(1,I));
      M2 = find(NUMMER(:,K) == e(2,I));
      if isempty(M1) + isempty(M2) == 0
         AUX(I) = K;
      end
   end
end
[wx,wy] = pdegrad(p,t,w);
for I = 1:N
   K = e(1:2,I);
   X = p(1,K); Y = p(2,K);
   X21 = X(2) - X(1); Y21 = Y(2) - Y(1);
   NN(:,I) = [Y21; -X21]/sqrt(X21*X21 + Y21*Y21);
   w_n(I)  = NN(1,I)*wx(AUX(I)) + NN(2,I)*wy(AUX(I));
end
