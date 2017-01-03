%6.5  convdiffcode.m

%written by Yeunwoo Cho, MIT 8/4/07%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Initial hat function%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
L=10;
dx=0.001;
dt=0.01;
c=1;
d=0.003;

x0=[-L+dx:dx:0]';
x1=[dx:dx:1]';
x2=[1+dx:dx:2]';

u0=zeros(1,length(x0));
u1=x1;
u2=2-x2;

x=[x0' x1' x2']';
n=length(x);

u=[u0 u1' u2']';
plot(x,u)
hold on
%P>1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
L=10;
dx=0.1;
dt=0.01;
c=1;
d=0.003;

x0=[-L+dx:dx:0]';
x1=[dx:dx:1]';
x2=[1+dx:dx:2]';

u0=zeros(1,length(x0));
u1=x1;
u2=2-x2;

x=[x0' x1' x2']';
n=length(x);

u=[u0 u1' u2']';

r=c*dt/dx;
R=d*(dt/(dx^2));
P=r/(2*R)

A_cde=sparse(diag((R+0.5*r)*ones(n-1,1),1)+...
             diag((1-2*R)*ones(n,1))+...
             diag((R-0.5*r)*ones(n-1,1),-1));
for t=dt:dt:7
    u=A_cde*u;
    if fix(t)==7;
        plot(x,u)
        axis([-10 2 -0.5 1.5]);
    end
end
%P<1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
L=10;
dx=0.1;
dt=0.01;
c=1;
d=0.07;

x0=[-L+dx:dx:0]';
x1=[dx:dx:1]';
x2=[1+dx:dx:2]';

u0=zeros(1,length(x0));
u1=x1;
u2=2-x2;

x=[x0' x1' x2']';
n=length(x);

u=[u0 u1' u2']';

r=c*dt/dx;
R=d*(dt/(dx^2));
P=r/(2*R)

A_cde=sparse(diag((R+0.5*r)*ones(n-1,1),1)+...
             diag((1-2*R)*ones(n,1))+...
             diag((R-0.5*r)*ones(n-1,1),-1));
for t=dt:dt:7
    u=A_cde*u;
    if fix(t)==7;
        plot(x,u,'r')
        axis([-10 2 -0.5 1.5]);
    end
end
%R+2r<1%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
L=10;
dx=0.1;
dt=0.01;
c=1;
d=0.1;

x0=[-L+dx:dx:0]';
x1=[dx:dx:1]';
x2=[1+dx:dx:2]';

u0=zeros(1,length(x0));
u1=x1;
u2=2-x2;

x=[x0' x1' x2']';
n=length(x);

u=[u0 u1' u2']';

r=c*dt/dx;
R=d*(dt/(dx^2));
r+2*R

A_cde=sparse(diag((R+r)*ones(n-1,1),1)+...
             diag((1-r-2*R)*ones(n,1))+...
             diag((R)*ones(n-1,1),-1));
for t=dt:dt:7
    u=A_cde*u;
    if fix(t)==7;
        plot(x,u,'r')
        axis([-10.2 2.2 -0.2 1.2]);
    end
end
text(-6.4, 0.1,sprintf('u(x,7)','fontsize',14,'bold'))
text(0.6,0.1,sprintf('u(x,0)','fontsize',14,'bold'))
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

