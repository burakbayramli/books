%do_shoot  to solve BVP2 by the shooting method
clear
figure(1), clf
t0=0; x0=1/4; dx0=0; tf=1; xf=1/3; N=100; tol=1e-8; kmax=10; Kg=4;
[t,x]= bvp2_shoot('df661',t0,tf,x0,xf,N,tol,kmax,Kg); %adjusting angle
xo=1./(4-t.*t); norm(x(:,1)-xo)/(N+1)
plot(t,xo,'r'), pause, clf
[t,x]= bvp2_shootp('df661',t0,tf,dx0,xf,N,tol,kmax,Kg); %adjusting position
xo=1./(4-t.*t); %the true analytical solution
norm(x(:,1)-xo)/(N+1) %discrepancy from the true analytical sol
cf=[2 -3 0];
norm(x(:,1)-xo)/(N+1)
plot(t,xo,'r'), pause , clf
t0=1; x0=5; dx0=-7; tf=2; xf=3; N=100; tol=.0001; kmax=10;
%[t,x]= bvp2_shoot('df662',t0,tf,x0,xf,N,tol,kmax); %adjusting angle
[t,x]= bvp2_shootp('df662',t0,tf,dx0,xf,N,tol,kmax); %adjusting position
xo=4./t./t +t; norm(x(:,1)-xo)
pause, plot(t,xo,'r')
%[t,x]= bvp2m_shoot('df662',t0,tf,x0,cf,N,tol,kmax);
%xt= dsolve('Dx1 = x2, Dx2 = (2*x1+4*t*x2)*x1', 'x1(0)=1/4, x2(1)=1/3');
%xt1=xt.x1, xt2=xt.x2