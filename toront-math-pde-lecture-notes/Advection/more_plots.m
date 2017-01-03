clear

t_0 = 0; t_f = 5; N = 200; M = 500;
[u,u_exact,x,t] = explicit_upwind(@f,t_0,t_f,M,N);
figure(1)
clf
plot(x,u(:,101),'-','LineWidth',2);
hold on
plot(x,u_exact(:,101),'--','LineWidth',2)

axis([0,2*pi,-0.1,1.1])
print -dps eu_modified_eq.ps
figure(1)

c = 1/2;
h = x(2)-x(1);
k = t(2)-t(1);

D = (c*h/2)*(1-c*k/2);
X = x-pi+1-c*t(101);
Y = 1/2 - 1/2*erf(X/sqrt(4*D*t(101)));
plot(x,Y,'o')

figure(2)
plot(x,u(:,101)-u_exact(:,101),'LineWidth',2)
hold on
plot(x,u(:,101)-Y,':','LineWidth',2)
hold off
axis([0,2*pi,-0.5,.5])
print -dps eu_modified_eq_errors.ps

