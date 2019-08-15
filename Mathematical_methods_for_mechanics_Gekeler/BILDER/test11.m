function test10
% kubisches Hermite-Polynom
U = rand(4);
u1 = U(1); u1_1 = U(2);
u2 = U(3); u2_1 = U(4);
x1 = 2; x2 = 6.23;
L = x2 - x1;
x = x1;

a = u1; b = u1_1;
c = (3*(u2 - u1) - L*(2*u1_1 + u2_1))/L^2;
d = (2*(u1- u2) + L*(u1_1 + u2_1))/L^3;
p = a + b*(x-x1) + c*(x - x1)^2 + d*(x- x1)^3;
p_1 = b + 2*c*(x - x1) + 3*d*(x - x1)^2;
diff1 = u1 - p
diff2 = u1_1 - p_1
