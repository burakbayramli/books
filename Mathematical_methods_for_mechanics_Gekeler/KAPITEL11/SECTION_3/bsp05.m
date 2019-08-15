function y = bsp04(t,x,parmtr3)
% Euler-Lagrange-Gleichungen fuer Pendel
% ergeben KEINE Loesung TODO--TODO
g = parmtr3(1); m = parmtr3(2); L = parmtr3(3);
y = zeros(4,1);
y(1:2) = x(3:4);
y(3)   = 0;
y(4)   = - g;
