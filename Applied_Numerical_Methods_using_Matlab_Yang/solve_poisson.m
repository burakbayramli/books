%solve_poisson in Example 9.1
f=inline('0','x','y');  g=inline('0','x','y');
x0=0; xf=4; Mx=20;  y0=0; yf=4; My=20;
bx0=inline('exp(y)-cos(y)','y');               %(E9.1-2a)
bxf=inline('exp(y)*cos(4)-exp(4)*cos(y)','y'); %(E9.1-2b)
by0=inline('cos(x)-exp(x)','x');               %(E9.1-3a)
byf=inline('exp(4)*cos(x)-exp(x)*cos(4)','x'); %(E9.1-3b)
D=[x0 xf y0 yf];  MaxIter=500;  tol=1e-4;
[U,x,y]= poisson(f,g,bx0,bxf,by0,byf,D,Mx,My,tol,MaxIter);
clf, mesh(x,y,U), axis([0 4 0 4 -100 100])
