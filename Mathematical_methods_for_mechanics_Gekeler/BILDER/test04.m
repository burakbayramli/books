%test04
d = 1/(2 + sqrt(2));
syms x y z1 z2 u
y = 1 + (1-2*d)*x + (d*d - 2*d + 0.5)*x*x;
z1 = 1 + 2*d*x - d*d*x*x + 4*d*d*x*x + 4*d*d*d*x*x*x;
z2 = d^4*x^4 + 8*d^3*x^3 - 3*4*d^4*x^4 + 16*d^4*x^4;
u = y*(z1 + z2);
u = simple(u)
