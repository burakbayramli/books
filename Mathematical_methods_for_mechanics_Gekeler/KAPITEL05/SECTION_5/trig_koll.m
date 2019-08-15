function [V,C] = trig_koll(U);
% Computation of C and C*U with trig. collocation
[p,n] = size(U); m = n/2; tau = 2*pi/(2*m);
Q = zeros(2*m,2*m);
for K = 1:2*m
   for J = 1:2*m
      Q(K,J) = exp(sqrt(-1)*(K-1)*(J-m)*tau);
   end
end
D1 = [1:m-1]; D2 = fliplr(D1); D = [-D2,0,D1,0];
D    = i*diag(D);
C  = Q*D*Q'/(2*m);
C  = real(C);
C1 = kron(C,eye(p));
W    = C1*U(:);
V    = reshape(W,p,n);
