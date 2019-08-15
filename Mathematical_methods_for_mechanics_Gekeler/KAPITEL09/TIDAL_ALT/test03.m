function test03
% Testen der exakten Loesung nach Lynch
clc
syms C D omga L b x t h g
z = C*cos(b*(L-x))*cos(omga*t);
u = - D*sin(b*(L-x))*sin(omga*t);
w = h + z;
z_t = diff(z,'t');
z_t = simplify(z_t);
z_x = diff(z,'x');
z_x = simplify(z_x);
wu = w*u;
wu_x = diff(wu,'x');
wu_x = simplify(wu_x);
res1 = z_t + wu_x;
res1 = simplify(res1)
u_t = diff(u,'t');
u_t = simplify(u_t);
u_x = diff(u,'x');
u_x = simplify(u_x);
uu_x = u*u_x;
uu_x = simplify(uu_x);
res2 = u_t + uu_x + g*z_x;
res2 = simplify(res2)


