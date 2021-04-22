L=10;
T=10;
c=1.5;
N=100;
h=L/N;      % Horizontal resolution (Dx)
T=10;M=1600;
%T=100;M=4000;
%  Test: increase duration of simulation, to see effect of dispersion and
%  effective wavenumber/speed (due to 2nd order scheme)
%T=10; M=1600; %T=40;M=8000; %T=100;M=4000; %N=50; T=100;M=4000;

k=T/M;      % Time resolution  (Dt)
C=c*k/h     % Try a case where C>1, e.g. decrease Dx or increase Dt
Lf=0.5;

x=[0:h:L]';
t=[0:k:T];
%fx=['exp(-0.5*(' num2str(L/2) '-x).^2/(' num2str(Lf) ').^2)']; 
%gx='0';
fx='exp(-0.5*(5-x).^2/0.5^2).*cos((x-5)*pi)';
gx='0';
f=inline(fx,'x');
g=inline(gx,'x');

figure(1)
plot(x,f(x));
a=title(['fx = ' fx]);
set(a,'FontSize',16);

n=length(x);
m=length(t);
u=zeros(n,m);
%Second order starter
u(2:n-1,1)=f(x(2:n-1));
for i=2:n-1
 u(i,2) = (1-C^2)*u(i,1) + k*g(x(i)) +C^2*(u(i-1,1)+u(i+1,1))/2;
end

%CDS: Iteration in time (j) and space (i)
for j=2:m-1
    for i=2:n-1
u(i,j+1)=(2-2*C^2)*u(i,j) + C^2*(u(i+1,j)+u(i-1,j)) - u(i,j-1);
    end
end
figure(5)
wavei(u',x,t);
a=xlabel('x');
set(a,'Fontsize',14);
a=ylabel('t');
set(a,'Fontsize',14);
a=title('Waves on String');
set(a,'Fontsize',16);
colormap;

%dn=floor(n/10);
%figure(3)
%atsplot(u(1:dn:n,:),1,t);
%a=xlabel('t');
%set(a,'Fontsize',14);
%a=ylabel('x');
%set(a,'Fontsize',14);
%a=title('Waves on String');
%set(a,'Fontsize',16);

