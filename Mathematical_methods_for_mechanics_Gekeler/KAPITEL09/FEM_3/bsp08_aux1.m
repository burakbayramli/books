function bsp08_aux1
% Maple-File fuer Beispiel 08
clc
syms x y; FAC = 10;
z = - 8*(x - x*x)^2*(y - y*y)^2;
w = 16*((6*x*x - 6*x + 1)*(y - y*y)^2 + (x - x*x)^2*(6*y*y - 6*y + 1));
u = -16*(x - x*x)^2*(y - y*y)*(1 - 2*y);
v =  16*(y - y*y)^2*(x - x*x)*(1 - 2*x);

rr1 = - 16*(x-x*x)^2*(y - y*y)*(1-2*y)* ...
    (16*(12*x - 6)*(y-y*y)^2 + 32*(x-x*x)*(6*y*y - 6*y + 1)*(1-2*x));
rr2 =   16*(y-y*y)^2*(x - x*x)*(1-2*x)* ...
   (16*(12*y - 6)*(x-x*x)^2 + 32*(y-y*y)*(6*x*x - 6*x + 1)*(1-2*y));
   rr = rr1 + rr2;
z = FAC*z; w = FAC*w; u = FAC*u; v = FAC*v; rr = FAC*FAC**rr;
wx = diff(w,'x');
wxx = diff(wx,'x')
wy = diff(w,'y');
wyy = diff(wy,'y')
UWX = u*wx;
UWY = v*wy;
RR = u*wx + v*wy - rr;
RR = simplify(RR);
uy = diff(u,'y');
vx = diff(v,'x');
DELTAZ = -vx + uy;
DIFF = -vx + uy + w;
DIFF = simplify(DIFF);
ux = diff(u,'x');
vy = diff(v,'y');
DIV = ux + vy;
DIV = simplify(DIV);
zx = diff(z,'x');
zy = diff(z,'y');
ZY_U = zy - u;
ZX_V = zx + v;
