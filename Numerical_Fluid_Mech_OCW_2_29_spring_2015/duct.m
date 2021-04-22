Lx=1;
Ly=1;
N=10;
h=Lx/N;
M=floor(Ly/Lx*N);
niter=20;
eps=1e-6;

x=[0:h:Lx]';
y=[0:h:Ly];
f1x='4*x-4*x.^2';
%f1x='0'
f2x='0';
g1x='0';
g2x='0';
gxy='0';
%gxy='-20*exp(-((0.5-x)/0.2).^2)*exp(-((0.5-y)/0.1).^2)';
f1=inline(f1x,'x');
f2=inline(f2x,'x');
g1=inline(g1x,'y');
g2=inline(g2x,'y');
gf=inline(gxy,'x','y');

n=length(x);
m=length(y);
u=zeros(n,m);
u(2:n-1,1)=f1(x(2:n-1));
u(2:n-1,m)=f2(x(2:n-1));
u(1,1:m)=g1(y);
u(n,1:m)=g2(y);
for i=1:n
    for j=1:m
        g(i,j) = gf(x(i),y(j));
    end
end
figure(1)
plot(x,f1(x));
a=title(['f(x) = ' f1x]);
set(a,'FontSize',16);

figure(5)
surf(y,x,u);
shading interp;
a=ylabel('x');
set(a,'Fontsize',14);
a=xlabel('y');
set(a,'Fontsize',14);
a=title(['Poisson Equation - g = ' gxy]);
set(a,'Fontsize',16);

u_0=mean(u(1,:))+mean(u(n,:))+mean(u(:,1))+mean(u(:,m));
u(2:n-1,2:m-1)=u_0*ones(n-2,m-2);
omega=4/(2+sqrt(4-(cos(pi/(n-1))+cos(pi/(m-1)))^2))
for k=1:niter
    u_old=u;
    for i=2:n-1
        for j=2:m-1
            u(i,j)=(1-omega)*u(i,j)+omega*(u(i-1,j)+u(i+1,j)+u(i,j-1)+u(i,j+1)-h^2*g(i,j))/4;
        end
    end
    r=abs(u-u_old)/max(max(abs(u)));
    k,r
    if (max(max(r))<eps)
        break;
    end
end

figure(2)
wavei(u',x,y);
shading interp;
a=xlabel('x');
set(a,'Fontsize',14);
a=ylabel('y');
set(a,'Fontsize',14);
%a=title(['Flow in duct =']);
a=title(['Poisson Equation - g = ' gxy]);
set(a,'Fontsize',16);
colormap;

figure(3)
surf(y,x,u);
shading interp;
a=ylabel('x');
set(a,'Fontsize',14);
a=xlabel('y');
set(a,'Fontsize',14);
a=title(['Poisson Equation - g = ' gxy]);
set(a,'Fontsize',16);

figure(4)
mesh(y,x,u);
a=ylabel('x');
set(a,'Fontsize',14);
a=xlabel('y');
set(a,'Fontsize',14);
%a=title(['Forward Euler - r =' num2str(r)]);
a=title(['Poisson Equation - g = ' gxy]);
set(a,'Fontsize',16);


