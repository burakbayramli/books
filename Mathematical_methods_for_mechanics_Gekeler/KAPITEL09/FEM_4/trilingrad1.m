function [T_W,T_Z,T_T] = trilingrad(p,t,W,Z,T)
% Computation of Gradients of trilinear form
M  = size(t,2); N = size(p,2);
T_W = sparse(N,N); T_Z = T_W; T_T = T_W;

for I = 1:M
   K   = t(1:3,I);
   Z21 = Z(K(2))-Z(K(1)); Z31 = Z(K(3))-Z(K(1)); Z32 = Z(K(3))-Z(K(2));
   W21 = W(K(2))-W(K(1)); W31 = W(K(3))-W(K(1)); W32 = W(K(3))-W(K(2));
   T21 = T(K(2))-T(K(1)); T31 = T(K(3))-T(K(1)); T32 = T(K(3))-T(K(2));
   T_W(K,K) = T_W(K,K) + ones(3,1)*[-Z32, Z31,-Z21]/6;
   T_Z(K,K) = T_Z(K,K) + ones(3,1)*[ W32,-W31, W21]/6;
   T_T(K,K) = T_T(K,K) + ones(3,1)*[ T32,-T31, T21]/6;
end
