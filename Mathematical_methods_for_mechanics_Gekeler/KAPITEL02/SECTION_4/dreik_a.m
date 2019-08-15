function y = dreik_a(t,x,parmtr3)
% differential system for restricted three-body problem
% for calculation of Arenstorf orbits
% mue is relative Moon mass m2/(m1 + m2)
% x = [x; y; xprime; yprime]
mue  = parmtr3;
% mue = 0.01212856276;
mue1 = 1 - mue;
a  = x(1); b = x(2); c = x(3); d = x(4);
n1 = ((a + mue)^2 + b^2)^(3/2);
n2 = ((a - mue1)^2 + b^2)^(3/2);
y1 = c; y2 = d;
y3 = a + 2*d - mue1*(a + mue)/n1 - mue*(a - mue1)/n2;
y4 = b - 2*c - mue1*b/n1 - mue*b/n2;
y  = [y1;y2;y3;y4];
