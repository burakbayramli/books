function test02
% Example Hamel, S. 328.
syms t
c = t^2;
x = t/sqrt(1 + c^2);
x1 = diff(x,'t');
x2 = diff(x1,'t');
c1 = diff(c,'t');
c2 = diff(c1,'t');
a  = c1*c1/(1 + c^2);
a1 = diff(a,'t');
f1 = (x2 - a*x)*x1;
f1 = simplify(f1);
f2 = (x2*c + 2*x1*c1 + x*c2)*(c*x1 + x*c1);
f2 = simplify(f2);
f3 = - a1*x^2/2;
f3 = simplify(f3);
f = f1 + f2 + f3;
f = simplify(f)
