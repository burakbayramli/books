function test02
%Exact example, check for pressure condition
% - nu*delta u + grad u*u + grad p = f

syms x y b 
%p = 2*pi*cos(pi*x)*sin(pi*y);
%intxp = int(p,x,0,1);
%intp = int(intxp,y,-0.5,0.5)
p_x = -2*pi^2*sin(pi*x)*sin(pi*y);
p_y =  2*pi^2*cos(pi*x)*cos(pi*y);

u_1   =  b*sin(pi*x)*sin(pi*y);
u_1x  =  b*pi*cos(pi*x)*sin(pi*y); 
u_1xx = -b*pi^2*sin(pi*x)*sin(pi*y);
u_1y  =  b*pi*sin(pi*x)*cos(pi*y);
u_1yy = -b*pi^2*sin(pi*x)*sin(pi*y);

u_2   =  b*cos(pi*x)*cos(pi*y);
u_2x  = -b*pi*sin(pi*x)*cos(pi*y);
u_2xx = -b*pi^2*cos(pi*x)*cos(pi*y);
u_2y  = -b*pi*cos(pi*x)*sin(pi*y);
u_2yy = -b*pi^2*cos(pi*x)*cos(pi*y);

f1 = pi*b^2*cos(pi*x)*sin(pi*x);
f2 = -pi*b^2*sin(pi*y)*cos(pi*y) + 4*pi^2*cos(pi*x)*cos(pi*y);

res1 = - (1/b)*(u_1xx + u_1yy) + p_x - f1;
res2 = - (1/b)*(u_2xx + u_2yy) + p_y - f2;
res2 = simplify(res2);

grad1 = u_1x*u_1 + u_1y*u_2;
grad2 = u_2x*u_1 + u_2y*u_2;
grad2 = simplify(grad2);
resa = res1 + grad1;
resb = res2 + grad2;

resa = simplify(resa);
resb = simplify(resb);

ff = -pi*b^2*sin(pi*y)*cos(pi*y) + 4*pi^2*cos(pi*x)*sin(pi*y);
ffy = diff(ff,y)