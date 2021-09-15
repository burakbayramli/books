% calc_f_ex1.m
function [f,Jac] = calc_f_ex1(x);

f = zeros(2,1);
f(1) = 3*x(1)^3 + 4*x(2)^2 - 145;
f(2) = 4*x(1)^2 - x(2)^3 + 28;

Jac = zeros(2,2);
Jac(1,1) = 9*x(1)^2;  Jac(1,2) = 8*x(2);
Jac(2,1) = 8*x(1);  Jac(2,2) = -3*x(2)^2;

return;
