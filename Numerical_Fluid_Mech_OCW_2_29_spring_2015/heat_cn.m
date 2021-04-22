L=1;
T=0.333;
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
% set up Crank-Nickolson coef matrix

d=(2+2*r)*ones(n-2,1);
b=-r*ones(n-2,1);
c=b;
% LU factorization
[alf,bet]=lu_tri(d,b,c);

for j=1:m-1
    rhs=r*(u(1:n-2,j)+u(3:n,j)) +(2-2*r)*u(2:n-1,j);
    rhs(1) = rhs(1)+r*u(1,j+1);
    rhs(n-2)=rhs(n-2)+r*u(n,j+1);
% Forward substitution
z=forw_tri(rhs,bet);
% Back substitution
y_b=back_tri(z,alf,c);
    for i=2:n-1
      u(i,j+1)=y_b(i-1);
    end
end
figure(2)
wavei(u',x,t);
shading interp;
a=xlabel('x');
set(a,'Fontsize',14);
a=ylabel('t');
set(a,'Fontsize',14);
a=title(['Crank-Nicholson - r =' num2str(r)]);
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
a=title(['Crank-Nicholson - r =' num2str(r)]);
set(a,'Fontsize',16);

figure(4)
mesh(t,x,u);

a=ylabel('x');
set(a,'Fontsize',14);
a=xlabel('t');
set(a,'Fontsize',14);
a=title(['Crank-Nicholson - r =' num2str(r)]);
set(a,'Fontsize',16);

