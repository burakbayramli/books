% simple_cost_func.m
function [F,g,iOK] = simple_cost_func(x, ModelParam);

iOK = 0;
c = ModelParam.c;

dx1 = x(1) - 1;
dx2 = x(2) - 2;
g = zeros(size(x));

F = dx1^2 + 10*dx2^2 + c*(dx1^4) + c*(dx2^4);
g(1) = 2*dx1 + 4*c*dx1^3;
g(2) = 2*10*dx2 + 4*c*dx2^3;

iOK = 1;
return;
