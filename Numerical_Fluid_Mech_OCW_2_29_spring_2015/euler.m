x=[0:0.1:10];
y0=0;
y=0.5*x.^2+y0;
figure(1)
hold off
a=plot(x,y,'b');
set(a,'Linewidth',2)
%step size
h=1.0;
% Euler's method, forward finite difference
xt=[0:h:10];
N=length(xt);
yt=zeros(N,1);
yt(1)=y0;
for n=2:N
    yt(n)=yt(n-1)+h*xt(n-1)
end
hold on;grid on;
a=plot(xt,yt,'r');
set(a,'Linewidth',2)
% Runge Kutta
f=inline('x','x','y');
[xrk,yrk]=ode45(f,xt,y0);
a=plot(xrk,yrk,'.g');
set(a,'MarkerSize',30)

% Euler's method, central finite difference
icentral=0
if (icentral)
  yt=zeros(N,1);
  yt(1)=y0;
  for n=3:N
      yt(n)=yt(n-2)+2*h*xt(n-1)
  end
  hold on;grid on;
  a=plot(xt,yt,'-r');
  a=plot(xt,yt,'.r');
  set(a,'Linewidth',1.5)
  set(a,'MarkerSize',15)
end
