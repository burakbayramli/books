function bsp08_aux2
% Maple-File fuer Beispiel 08
clc
syms x y;
u = -16*(x - x*x)^2*(y - y*y)*(1 - 2*y);
v =  16*(y - y*y)^2*(x - x*x)*(1 - 2*x);

ux = diff(u,'x');
ux = simplify(ux);
uy = diff(u,'y');
uy = simplify(uy);
uxx = diff(ux,'x');
uyy = diff(uy,'y');

vx = diff(v,'x');
vx = simplify(vx);
vy = diff(v,'y');
vy = simplify(vy);
vxx = diff(vx,'x');
vyy = diff(vy,'y');

deltau = uxx + uyy;
deltav = vxx + vyy;
deltav_x = diff(deltav,'x');
deltau_y = diff(deltau,'y');
deltav_x = simplify(deltav_x)
deltau_y = simplify(deltau_y)
grad1 = ux*u+uy*v;
grad2 = vx*u+vy*v;
aux1 = diff(grad2,'x');
aux2 = diff(grad1,'y');
aux1 = simplify(aux1)
aux2 = simplify(aux2)
