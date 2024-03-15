% NUMERICAL SOLUTION OF Blasius's ODE:
% fppp + f fpp = 0,
% where fppp = d3fdn3 and fpp = d2fdn2.
% Boundary conditions:
% f = fp = 0 at n=0,
% where fp = dfdn, and
% f -> 1 as n -> infinity.
%
% Let v = fp = dfdn; thus, we can write an ODE for v, viz.:
% d2vdn2 +  f * dvdn = 0.
% This is done to help develop a numerical method to solve
% this two-point boundary-value problem.
clear;clc
dn = .1;
v(61) = 1;
f(61) = 1;
v(1) = 0;
f(1) = 0;
y(1) = 0;
for it = 1:150000
for m = 2:60
y(m) = y(m-1) + dn;
f(m) = (v(m)+v(m-1))*dn/2 + f(m-1);
v(m) = (v(m+1) + v(m-1))/2 + f(m) * dn* (v(m+1) - v(m-1))/4;
fpp(m-1) = (v(m)-v(m-1))/dn;
end
end
plot(v(1:end-1),y,'-ok')
title('Blasius''s laminar velocity profile')
xlabel('Horizontal velocity component, u/U_\infty')
ylabel('Similarity coordinate, \eta')
% The method applied is not sophisticated but it works. The
% results compare excellently with the results reported in
% White, F. M., VISCOUS FLUID FLOW, McGraw-Hill (1974).