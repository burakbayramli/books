function test03
% Test full brusselator
clc
flag = 0;
if flag == 1;
syms a u v w
x1 = 1 + u; x2 = a +v; x3 = a + w;
f1 = -x1^2*x2 + (x3 + 1)*x1 - 1;
f2 = -x1*x3 + x1^2*x2;
f3 = x1*x3 - a;
h1 = -a*u^2 - 2*u*v - u^2*v + u*w;
h2 = - h1;
h3 = u*w;
A = [1-a,-1,1;a,1,-1;a,0,1]; U = [u;v;w];
B = A*U;
C = B + [h1;h2;h3];
diff1 = C(1) - f1;
diff2 = C(2) - f2;
diff3 = C(3) - f3;
diff1 = simplify(diff1)
diff2 = simplify(diff2)
diff3 = simplify(diff3)
end
a = (9 - sqrt(17))/4
A = [1-a,-1,1;a,1,-1;a,0,1]; 

