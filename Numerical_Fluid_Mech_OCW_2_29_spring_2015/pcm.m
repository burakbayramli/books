func='4*exp(-0.8*x)-0.5*y';
f=inline(func,'x','y');
y0=2;
%step size
h=0.5;
% Euler's method, forward finite difference
xt=[0:h:10];
N=length(xt);
yt=zeros(N,1);
yt(1)=y0;
for n=2:N
    yt(n)=yt(n-1)+h*f(xt(n-1),yt(n-1));
end
hold off
a=plot(xt,yt,'r');
set(a,'Linewidth',2)
% Heun's method
xt=[0:h:10];
N=length(xt);
yt=zeros(N,1);
yt(1)=y0;
for n=2:N    
    yt_0=yt(n-1)+h*f(xt(n-1),yt(n-1));
    yt(n)=yt(n-1)+h*(f(xt(n-1),yt(n-1))+f(xt(n),yt_0))/2;
end
hold on
a=plot(xt,yt,'g');
set(a,'Linewidth',2)
% Runge Kutta
x=[0:0.1:10];
hold on
[xrk,yrk]=ode45(f,x,y0);
a=plot(xrk,yrk,'b');
set(a,'Linewidth',2)
a=title(['dy/dx = f(x,y) = ' func]);
set(a,'Fontsize',16);
a=xlabel('x');
set(a,'Fontsize',14);
a=ylabel('y');
set(a,'Fontsize',14);
a=legend('Euler','Heun','Exact');
set(a,'Fontsize',14);


