function y = cuconBasis1(x)
% cuconBasis1  Basis fcns for conductivity model:  1/k = c1/T + c2*T^2
y = [1./x  x.^2];
