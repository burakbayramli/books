function [f,dfdx] = fx3n(x)
% fx3n  Evaluate f(x) = x - x^(1/3) - 2 and dfdx for Newton algorithm
f    = x - x.^(1/3) - 2;
dfdx = 1 - (1/3)*x.^(-2/3);
