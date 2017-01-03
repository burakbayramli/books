%do_fdf to solve a linear BVP2 by the finite difference method
clear, clf
t0=1; x0=5; dx0=-7; tf=2; xf=3; N=500; 
a1=inline('2./t','t'); a0=inline('-2./t./t','t'); u=0;
[tt,x_fd]=bvp2_fdf(a1,a0,u,t0,tf,x0,xf,N);
[tt,x_fd1]=bvp2_fdfp(a1,a0,u,t0,tf,dx0,xf,N);
discrepancy=norm(x_fd-x_fd1)
c0=[1 0 x0]; c0=[2 1 3];  cf=[1 0 xf];
[tt,x_fdm]=bvp2m_fdf(a1,a0,u,t0,tf,x0,cf,N);
[tt,x_fdm1]=bvp2m_fdf1(a1,a0,u,t0,tf,x0,cf,N);
[tt,x_fdmm]=bvp2mm_fdf(a1,a0,u,t0,tf,c0,cf,N); 
[tt,x_fdmm1]=bvp2mm_fdf1(a1,a0,u,t0,tf,c0,cf,N);
%To check if the boundary conditions are satisfied by this solution
h=(tf-t0)/N;
c0*[x_fdmm(1) (x_fdmm(2)-x_fdmm(1))/h -1]' %=0?
cf*[x_fdmm(end) (x_fdmm(end)-x_fdmm(end-1))/h -1]' %=0?
plot(tt,x_fd,'b', tt,x_fdm,'r', tt,x_fdmm,'m')

xo=dsolve('D2x+2*(t*Dx-x)/t^2=0','x(1)=5, x(2)=3')
xot=subs(xo,'t',tt); %xot=4./tt./tt +tt; %true analytical solution
err_fd=norm(x_fd-xot)/(N+1), err_fd1=norm(x_fd1-xot)/(N+1)
err_fdm=norm(x_fdm-xot)/(N+1), err_fdmm=norm(x_fdmm-xot)/(N+1)
err_fdm1=norm(x_fdm1-xot)/(N+1), err_fdmm1=norm(x_fdmm1-xot)/(N+1)
hold on, pause, plot(tt,xot,'k')
x=dsolve('Dx1=x2','Dx2=2/t^2*x1-2*x2/t','x1(1)=5','x1(2)=3');
%x=dsolve('Dx1=x2','Dx2=2*(x1-t*x2)/t^2','x1(1)=5','x1(2)=3'); 
xo=x.x1

%Another Finite Difference Method capable of nonlinear BVP, but not so efficient
another_fdf=0;
if another_fdf>1
  figure(2), clf
  t0=0; x0=1/4;  tf=1; xf=1/3; N=1; kmax=1000;
  tol=10^-8;
  t= [t0 tf];  x=[x0 xf];
  for itr=1:8
    N= N*2;
    h=(tf-t0)/N;  ti= [t0: h: tf];
    x= interp1(t,x,ti); %x(1)=x0; x(N+1)=xf;
    t= ti;
    for k=1: kmax
      xp= x;
      for i=2:N
        x(i)= (x(i-1)+x(i+1))/2 -h*x(i)*(h*x(i)+t(i)*(x(i+1)-x(i-1)));  
      end
      if norm(x-xp)<tol, break; end
    end
    k, plot(t,x), hold on, pause
  end
  xo=1./(4-t.*t); norm(x-xo)/(N+1)
  plot(t,xo,'r')
end

%Another linear differential equation
t0=0; x0=1.25; tf=4; xf=-0.95; N=20; 
a1=inline('-2*t./(1+t.*t)','t'); a0=inline('2./(1+t.*t)','t'); u=1;
[t,x]=bvp2_fdf(a1,a0,u,t0,tf,x0,xf,N);
%plot(t,x)

x=dsolve('D2x-2*(t*Dx-x)/(t^2+1)=1','x(0)=1.25, x(4)=-0.95')
x=dsolve('Dx1=x2','Dx2=2*(t*x2-x1)/(t^2+1)+1','x1(0)=1.25','x1(4)=-0.95');
