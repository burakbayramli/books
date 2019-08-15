function test01
% Rechnungen fuer phys. Dppelpendel
syms f1 f2 df1 df2 ddf1 ddf2 el el1 el2
x3 = - el*sin(f1)*df1^2 + el*cos(f1)*ddf1 -el2*sin(f2)*df2^2+el2*cos(f2)*ddf2;
x4 = - el*cos(f1)*df1^2 - el*sin(f1)*ddf1 -el2*cos(f2)*df2^2- el2*sin(f2)*ddf2;
diff = cos(f1)*x4 - sin(f1)*x3;
diff1 = simplify(diff)
diff = cos(f2)*x3 - sin(f2)*x4;
diff2 = simplify(diff)
