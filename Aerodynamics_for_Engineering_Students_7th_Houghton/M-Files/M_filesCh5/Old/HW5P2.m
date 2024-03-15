% HW5 Problem 2 (Spring 2012)
syms x y mu 
phi = (mu/2/pi) * x/(x^2 + y^2)
psi = -(mu/2/pi) * y/(x^2 + y^2)
u1 =   diff(phi,x)
v1 =   diff(phi,y)
u2 =   diff(psi,y)
v2 = - diff(psi,x)
u1 = simple(u1)
u2 = simple(u2)
u1 == u2
v1 == v2
% Executing this script, the following was obtained"
% u1 = -(mu*(x^2 - y^2))/(2*pi*(x^2 + y^2)^2)
% v1 = -(mu*x*y)/(pi*(x^2 + y^2)^2)
% u2 = -(mu*(x^2 - y^2))/(2*pi*(x^2 + y^2)^2)
% v2 = -(mu*x*y)/(pi*(x^2 + y^2)^2)
% u1 == u2 ==> 1 (true)
% v1 == v2 ==> 1 (true)
% This is what we wanted to demonstrate.