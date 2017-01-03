%solve_heat2
clear, clf
a=1e-4; 
it0=inline('0','x','y'); %(E9.3-2a)
bxyt=inline('exp(y)*cos(x)-exp(x)*cos(y)','x','y','t'); %(E9.3-2b)
D=[0 4 0 4]; T=5000; Mx=40; My=40; N=50;
[u,x,y,t]=heat2_ADI(a,D,T,it0,bxyt,Mx,My,N);
mesh(x,y,u)
