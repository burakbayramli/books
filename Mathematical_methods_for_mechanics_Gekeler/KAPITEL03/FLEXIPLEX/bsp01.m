function Y = bsp01(X)
% Test function for minimization,
% Rosenbrock's -function
Y = 100*(X(2) - X(1)^2)^2 + (1 - X(1))^2;
