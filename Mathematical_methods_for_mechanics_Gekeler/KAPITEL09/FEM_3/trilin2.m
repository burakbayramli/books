function [WR,T_W,T_Z] = trilin2(p,q,W,Z);
% bilinear parallelogram elements for coupled system
% Trilinearform WR for right side and gradients 

N = size(p,2); WR = zeros(N,1);
T_W = sparse(N,N); T_Z = sparse(N,N);
for I = 1:size(q,2)
   K = q(1:4,I); X = p(1,K); Y = p(2,K);
   [C,D]    = trilin2_aux(X,Y,W(K),Z(K));
   T_W(K,K) = T_W(K,K) + C;
   T_Z(K,K) = T_Z(K,K) + D;
   WR(K)    = WR(K)    + C*W(K);
end
