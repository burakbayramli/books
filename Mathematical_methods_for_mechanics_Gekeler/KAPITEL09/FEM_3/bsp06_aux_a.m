function bsp06_aux_a
% Maple-File fuer Beispiel 06
clc
syms x y;
z = - 8*(x - x*x)^2*(y - y*y)^2;
w = 16*((6*x*x - 6*x + 1)*(y - y*y)^2 + (x - x*x)^2*(6*y*y - 6*y + 1));
u = -16*(x - x*x)^2*(y - y*y)*(1 - 2*y);
v =  16*(y - y*y)^2*(x - x*x)*(1 - 2*x);
% -- 0.k. -----------------------

wx = diff(w,'x')
wxx = diff(wx,'x');
wy = diff(w,'y')
wyy = diff(wy,'y');
deltaw = wxx + wyy;
deltaw = simplify(deltaw);
convecw = u*wx + v*wy;
convecw = simplify(convecw);
