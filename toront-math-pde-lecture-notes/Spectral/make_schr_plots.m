
t_0 = 0; t_f = 5; M = 10; N = 32; 
dt = (t_f-t_0)/M;
dx = 2*pi/N;
x = 0:dx:2*pi;
% choose how often I want a solution returned
NPrint = M;
% choose focussing (K=1) or defocussing (K=-1):
K = -1;
% set the plane wave initial data

A = 4;
B = 3; 
C = 0;
u0 = A*exp(i*(B*x+0*(-K*A^2+B^2)+C));
u0 = u0';
uf = A*exp(i*(B*x+t_f*(-K*A^2+B^2)+C));
uf = uf';
% initial data
% u0 = exp(exp(i*x));
% u0 = u0';

[u1,x,t1,kk,amp1,M1,H1] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u2,x,t2,kk,amp2,M2,H2] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u3,x,t3,kk,amp3,M3,H3] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u4,x,t4,kk,amp4,M4,H4] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u5,x,t5,kk,amp5,M5,H5] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u6,x,t6,kk,amp6,M6,H6] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

% for initial data
%    u0 = A*exp(i*(B*x+0*(-K*A^2+B^2)+C));
% make plots of the instability (defocussing case)
figure(1)
clf
plot(kk,amp5(:,1),'o-');
hold on
plot(kk,amp5(:,11),'o-');
plot(kk,amp5(:,21),'o-');
plot(kk,amp5(:,31),'o-');
print -dps split_schroedinger_unstable_spec.ps
 
figure(2)
clf
plot(kk,amp6(:,1),'o-');
hold on
plot(kk,amp6(:,21),'o-');
plot(kk,amp6(:,41),'o-');
plot(kk,amp6(:,61),'o-');
print -dps split_schroedinger_stable_spec.ps
