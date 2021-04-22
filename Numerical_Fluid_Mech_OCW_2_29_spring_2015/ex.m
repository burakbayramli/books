x=[0:0.01:1];
y0=1;
y=exp(x);
figure(1)
hold off
a=plot(x,y,'b');
set(a,'Linewidth',2)
%step size
h=0.05;
% Euler's method, forward finite difference
xt=[0:h:1];
N=length(xt);
yt=zeros(N,1);
yt(1)=y0;
for n=2:N
    yt(n)=yt(n-1)+h*exp(xt(n-1));
end
hold on
a=plot(xt,yt,'+r');
set(a,'MarkerSize',12)
% Runge Kutta
fxy='exp(x)'
f=inline(fxy,'x','y');
[xrk,yrk]=ode45(f,xt,y0);
%a=plot(xrk,yrk,'.g');
%set(a,'MarkerSize',30);
a=title(['dy/dx = ' fxy ', y_0 = ' num2str(y0)])
set(a,'FontSize',16);
%b=legend('Exact',['Euler, h=' num2str(h)],'Runge-Kutta (Matlab)');
b=legend('Exact',['Euler, h=' num2str(h)]);
set(b,'FontSize',14);
