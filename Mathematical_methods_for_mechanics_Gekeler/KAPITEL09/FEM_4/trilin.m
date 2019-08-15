function WR = trilin(p,t,W,Z)
% Computation of trilinear form

N = size(p,2); M  = size(t,2); WR = zeros(N,1);
for I = 1:M
   K     = t(1:3,I); WW = W(K); ZZ = Z(K);
   AUX1  = ZZ(2)*WW(1) + ZZ(3)*WW(2) + ZZ(1)*WW(3);
   AUX2  = ZZ(3)*WW(1) + ZZ(1)*WW(2) + ZZ(2)*WW(3);
   WR(K) = (AUX1 - AUX2)*ones(3,1)/6;
end
