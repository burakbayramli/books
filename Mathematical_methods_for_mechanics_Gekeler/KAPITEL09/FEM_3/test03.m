function test03
% Ableitungen fuer Testbeispiel
clc
syms x y

u = -16*(x - x*x)^2*(y - y*y)*(1 - 2*y);
v =  16*(y - y*y)^2*(x - x*x)*(1 - 2*x);
ux = diff(u,'x');    ux = simplify(ux)
uy = diff(u,'y');    uy = simplify(uy);
uxx = diff(ux,'x'); uxx = simplify(uxx);
uyy = diff(uy,'y'); uyy = simplify(uyy);

vx = diff(v,'x');    vx = simplify(vx);
vy = diff(v,'y');    vy = simplify(vy);
vxx = diff(vx,'x'); vxx = simplify(vxx);
vyy = diff(vy,'y'); vyy = simplify(vyy)

