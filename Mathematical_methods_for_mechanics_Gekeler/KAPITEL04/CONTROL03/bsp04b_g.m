function DYDT = bsp04b_g(T,Y);
% Bryson-Ho, par. 2.4
% Costate equations, same as bsp04a.m

DYDT = zeros(4,1);
DYDT(1) = 0;
DYDT(2) = 0;
DYDT(3) = - Y(1);
DYDT(4) = - Y(2);
