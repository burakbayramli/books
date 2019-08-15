function bsp07_aux
% Maple-File fuer Beispiel 07
clc
syms x y;
z = - 8*(x - x*x)^2*(y - y*y)^2;
w = 16*((6*x*x - 6*x + 1)*(y - y*y)^2 + (x - x*x)^2*(6*y*y - 6*y + 1));
u = -16*(x - x*x)^2*(y - y*y)*(1 - 2*y);
v =  16*(y - y*y)^2*(x - x*x)*(1 - 2*x);

wx = diff(w,'x');
wxx = diff(wx,'x');
wy = diff(w,'y');
wyy = diff(wy,'y');

tx1 = - wxx - wyy;
tx1 = simplify(tx1);
t1 = int(tx1,'x');
t1 = simplify(t1)

tx2 = u*wx + v*wy;
tx2 = simplify(tx2);
t2 = int(tx2,'x');
t2 = simplify(t2)

t = (t1 + t2);
tx = diff(t,'x');
tx = simplify(tx);
txx = diff(tx,'x');
txx = simplify(txx);
ty = diff(t,'y');
ty = simplify(ty);
tyy = diff(ty,'y');
tyy = simplify(tyy);

delta_t = txx + tyy;
delta_t = simplify(delta_t)

convec_t = u*tx + v*ty;
convec_t = simplify(convec_t)

