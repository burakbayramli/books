clear
rho=1000;
Cd=1;
m=5;
r=0.05;
fac=rho*Cd*pi*r^2/(2*m);
v=1;
save dragpar ;

x=[0:0.1:10];
%step size
h=1.0;
% Euler's method, forward finite difference
%t=[0:h:10];
t=[0:h:20];
N=length(t);
u_e=zeros(N,1);
x_e=zeros(N,1);
u_e(1)=0;
x_e(1)=0;
for n=2:N
    u_e(n)=u_e(n-1)+h*fac*(v^2-2*v*u_e(n-1)+u_e(n-1)^2);
    x_e(n)=x_e(n-1)+h*u_e(n-1);
end
% Runge Kutta
u0=[0 0]';
[tt,u]=ode45(@dudt,t,u0);

figure(1)
hold off
a=plot(t,u_e,'+b');
set(a,'Markersize',10)
hold on
a=plot(tt,u(:,1),'.g');
set(a,'Markersize',15)
set(a,'Linewidth',2)
a=plot(tt,abs(u(:,1)-u_e),'+r');
set(a,'Markersize',10)
a=legend('Eulers Method','Exact','Error','Location','East');
set(a,'Fontsize',14);
a=title(['Vel. of Sphere in Flow - \Delta t =' num2str(h)]);
set(a,'Fontsize',16);
a=xlabel('Time (s)');
set(a,'Fontsize',14);
a=ylabel('u (m/s)');
set(a,'Fontsize',14);

figure(2)
hold off
a=plot(t,x_e,'+b');
set(a,'Markersize',10);
set(a,'Linewidth',2);
hold on
a=plot(tt,u(:,2),'.g');
set(a,'Markersize',15)
a=plot(tt,abs(u(:,2)-x_e),'xr');
set(a,'Markersize',10);
a=legend('Eulers Method','Exact','Error','Location','NorthWest');
set(a,'Fontsize',14);
a=title(['Pos. of Sphere in Flow - \Delta t =' num2str(h)]);
set(a,'Fontsize',16);
a=xlabel('Time (s)');
set(a,'Fontsize',16);
a=ylabel('x (m)');
set(a,'Fontsize',16);