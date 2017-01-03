%solve_heat
a=1; %the parameter of (E9.2-1)
it0=inline('sin(pi*x)','x'); %initial condition
bx0=inline('0');  bxf=inline('0'); %boundary condition
xf=1; M=25;  T=0.1; N=100; %r=0.625 
%analytical solution
uo=inline('sin(pi*x)*exp(-pi*pi*t)','x','t'); 
[u1,x,t]=heat_exp(a,xf,T,it0,bx0,bxf,M,N); 
figure(1), clf, mesh(t,x,u1)
[u2,x,t]=heat_imp(a,xf,T,it0,bx0,bxf,M,N); %converge unconditionally
figure(2), clf, mesh(t,x,u2)
[u3,x,t]=heat_CN(a,xf,T,it0,bx0,bxf,M,N); %converge unconditionally
figure(3), clf, mesh(t,x,u3)
MN=M*N;
Uo= uo(x,t); aUo=abs(Uo)+eps; %values of true analytical solution
%How far from the analytical solution?
err1= norm((u1-Uo)./aUo)/MN
err2= norm((u2-Uo)./aUo)/MN
err3= norm((u3-Uo)./aUo)/MN
