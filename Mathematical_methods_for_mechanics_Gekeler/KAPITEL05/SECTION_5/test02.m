function test02
clc
syms a u v mu
x = a + u; y = v + (a*a + 1 + mu)/a;
f1 = (a*a + 2 + mu)*x - x*x*y -a;
g1 = -(a*a + 1 + mu)*x + x*x*y;
f2 = -(a*a+mu)*u-a*a*v - (a+mu/a + 1/a)*u*u - 2*a*u*v - u*u*v;
g2 = (a*a +1 +mu)*u + a*a*v + (a + mu/a + 1/a)*u*u + 2*a*u*v + u*u*v;
diff1 = f1 - f2;
diff1 = simplify(diff1)
diff2 = g1 - g2;
diff2 = simplify(diff2)





