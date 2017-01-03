clear

t_0 = 0; t_f = 5; N = 200; M = 500;
[u,u_exact,x,t] = explicit_upwind(@f,t_0,t_f,M,N);
figure(1)
clf
plot(x,u(:,101),'-','LineWidth',2);
hold on
plot(x,u_exact(:,101),'--','LineWidth',2)
hold off
axis([0,2*pi,-0.1,1.1])
title('Explicit Upwind','FontSize',16)
print -dps explicit_upwind.ps
figure(1)

[u,u_exact,x,t] = lax_friedrichs(@f,t_0,t_f,M,N);
figure(2)
clf
plot(x,u(:,101),'-','LineWidth',2);
hold on
plot(x,u_exact(:,101),'--','LineWidth',2)
hold off
axis([0,2*pi,-0.1,1.1])
title('Lax-Friedrichs','FontSize',16);
print -dps lax_friedrichs.ps
figure(2)

[u,u_exact,x,t] = lax_wendroff(@f,t_0,t_f,M,N);
figure(3)
clf
plot(x,u(:,101),'-','LineWidth',2);
hold on
plot(x,u_exact(:,101),'--','LineWidth',2)
hold off
axis([0,2*pi,-0.3,1.3])
title('Lax-Wendroff','FontSize',16)
print -dps lax_wendroff.ps
figure(3)

[u,u_exact,x,t] = beam_warming(@f,t_0,t_f,M,N);
figure(4)
clf
plot(x,u(:,101),'-','LineWidth',2);
hold on
plot(x,u_exact(:,101),'--','LineWidth',2)
hold off
axis([0,2*pi,-0.3,1.3])
title('Beam Warming','FontSize',16)
print -dps beam_warming.ps
figure(4)

