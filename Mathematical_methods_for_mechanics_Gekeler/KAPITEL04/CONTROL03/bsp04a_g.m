function DYDT = bsp04a_g(T,Y);
% Bryson-Ho, Par. 2.4
% Costate equations

DYDT = zeros(4,1);
DYDT(1) = 0;
DYDT(2) = 0;
DYDT(3) = - Y(1);
DYDT(4) = - Y(2);
