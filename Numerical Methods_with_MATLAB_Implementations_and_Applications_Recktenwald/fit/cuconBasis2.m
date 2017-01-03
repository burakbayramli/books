function y = cuconBasis2(x)
% cuconBasis2  Basis fcns for conductivity model:  1/k = c1/T + c2*T + c3*T^2
y = [1./x  x  x.^2];
