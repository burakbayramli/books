%solve_wave2
it0=inline('0.1*sin(pi*x)*sin(pi*y/2)','x','y'); %(E9.5-3)
i1t0=inline('0','x','y');  bxyt=inline('0','x','y','t'); %(E9.5-2)
a=.25; D=[0 2 0 2]; T=2; Mx=40; My=40; N=40;
[u,x,y,t]=wave2(a,xf,T,it0,i1t0,bxyt,Mx,My,N);
