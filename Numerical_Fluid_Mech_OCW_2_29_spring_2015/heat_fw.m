L=1;
T=0.2;
c=1;
N=5;
h=L/N;
M=10;
k=T/M;
r=c^2*k/h^2

x=[0:h:L]';
t=[0:k:T];
fx='4*x-4*x.^2';
g1x='0';
g2x='0';
f=inline(fx,'x');
g1=inline(g1x,'t');
g2=inline(g2x,'t');
figure(1)
plot(x,f(x));
a=title(['fx = ' fx]);
set(a,'FontSize',16);

n=length(x);
m=length(t);
u=zeros(n,m);
u(2:n-1,1)=f(x(2:n-1));
u(1,1:m)=g1(t);
u(n,1:m)=g2(t);

for j=1:m-1
    for i=2:n-1
u(i,j+1)=(1-2*r)*u(i,j) + r*(u(i+1,j)+u(i-1,j));
    end
end
figure(2)
wavei(u',x,t);
shading interp;
a=xlabel('x');
set(a,'Fontsize',14);
a=ylabel('t');
set(a,'Fontsize',14);
a=title(['Forward Euler - r =' num2str(r)]);
%a=title('Heat Equation');
set(a,'Fontsize',16);
colormap;

dn=floor(n/10);
figure(3)
surf(t,x,u);
shading interp;
a=ylabel('x');
set(a,'Fontsize',14);
a=xlabel('t');
set(a,'Fontsize',14);
a=title(['Forward Euler - r =' num2str(r)]);
%a=title('Heat Equation');
set(a,'Fontsize',16);

figure(4)
mesh(t,x,u);
a=ylabel('x');
set(a,'Fontsize',14);
a=xlabel('t');
set(a,'Fontsize',14);
a=title(['Forward Euler - r =' num2str(r)]);
%a=title('Heat Equation');
set(a,'Fontsize',16);


