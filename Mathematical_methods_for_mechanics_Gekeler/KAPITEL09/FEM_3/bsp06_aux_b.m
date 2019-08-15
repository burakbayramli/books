function bsp06_aux_b
% Maple-File fuer Beispiel 06
% andere rechte Seite
clc
syms x y; FAC = 10;
z = - 8*(x - x*x)^2*(y - y*y)^2;
w = 16*((6*x*x - 6*x + 1)*(y - y*y)^2 + (x - x*x)^2*(6*y*y - 6*y + 1));
u = -16*(x - x*x)^2*(y - y*y)*(1 - 2*y);
v =  16*(y - y*y)^2*(x - x*x)*(1 - 2*x);

ux = diff(u,'x');
ux = simplify(ux);
uxx = diff(ux,'x');
uxx = simplify(uxx);
uy = diff(u,'y');
uy = simplify(uy);
uyy = diff(uy,'y');
uyy = simplify(uyy);
deltau = uxx + uyy;
deltau = simplify(deltau);

vx = diff(v,'x');
vx = simplify(vx);
vxx = diff(vx,'x');
vxx = simplify(vxx);
vy = diff(v,'y');
vy = simplify(vy);
vyy = diff(vy,'y');
vyy = simplify(vyy);
deltav = vxx + vyy;
deltav = simplify(deltav);
convecu = ux*u + uy*v;
convecu = simplify(convecu);
convecv = vx*u + vy*v;
convecv = simplify(convecv);
deltau_y = diff(deltau,'y');
deltau_y = simplify(deltau_y);
deltav_x = diff(deltav,'x');
deltav_x = simplify(deltav_x);
rotf1 = deltav_x - deltau_y;
rotf1 = simplify(rotf1)
convecu_y = diff(convecu,'y');
convecu_y = simplify(convecu_y);
convecv_x = diff(convecv,'x');
convecv_x = simplify(convecv_x);
rotf2 = convecv_x - convecu_y;
rotf2 = simplify(rotf2)
