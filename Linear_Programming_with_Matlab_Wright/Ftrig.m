function Fval = Ftrig(z)
% evaluates the three-dimensional example at the vector x in R^3
Fval = [z(1)^2 + z(2)^2 - pi^2; z(3) - cos(z(1)); z(3) - sin(z(2))];

