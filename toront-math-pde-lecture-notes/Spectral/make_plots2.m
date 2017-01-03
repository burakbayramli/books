
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explicit timestepping
clear
t_0 = 0; t_f = 1; M = 800; N = 64;
h = 2*pi/N;
x = 0:h:2*pi-h;

eps = 1;
u0 = 1./(eps^2 + sin(x/2).^2);

[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);

figure(1)
clf
hold on
plot(x,u(:,1),'LineWidth',2);
axis([0,2*pi,.45,1.05])
plot(x,u(:,201),'--','LineWidth',2);
plot(x,u(:,401),'-.','LineWidth',2);
plot(x,u(:,601),':','LineWidth',2);
plot(x,u(:,801));
print -dps decaying_solution.ps

figure(2)
clf
hold on
plot(kk,amp(:,1),'o-','LineWidth',2);
axis([0,max(kk),-17,1])
plot(kk,amp(:,201),'o--','LineWidth',2);
plot(kk,amp(:,401),'o-.','LineWidth',2);
plot(kk,amp(:,601),'o:','LineWidth',2);
plot(kk,amp(:,801),'o-');
print -dps decaying_spectrum.ps
% times 0, .25, .5, .75, 1

%%%%%%%%%%%%%%%%%%%  what if time steps are too large?

t_0 = 0; t_f = 1; M = 450; N = 64;
[u,err,x,t,kk,amp] = heat4(t_0,t_f,M,N,u0);

figure(1)
clf
hold on
plot(x,u(:,1),'LineWidth',2);
plot(x,u(:,114),'--','LineWidth',2);
plot(x,u(:,226),'-.','LineWidth',2);
plot(x,u(:,280),':','LineWidth',2);
print -dps unstable_solution.ps

figure(2)
clf
hold on
plot(kk,amp(:,1),'o-','LineWidth',2);
axis([0,max(kk),-17,1])
plot(kk,amp(:,114),'o--','LineWidth',2);
plot(kk,amp(:,226),'o-.','LineWidth',2);
plot(kk,amp(:,280),'o:','LineWidth',2);
print -dps unstable_spectrum.ps
% times are 0, .2511, 1/2, .62, 
