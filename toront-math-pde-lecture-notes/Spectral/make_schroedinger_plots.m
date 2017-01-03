% for plane wave initial data:
t_0 = 0; t_f = 5; M = 10; N = 32; 
dt = (t_f-t_0)/M;
NPrint = M;
% choose focussing (K=1) or defocussing (K=-1):
K = -1;
dx = 2*pi/N;
x = 0:dx:2*pi;
A = 4;
B = 3; 
C = 0;
u0 = A*exp(i*(B*x+0*(-K*A^2+B^2)+C));
u0 = u0';
uf = A*exp(i*(B*x+t_f*(-K*A^2+B^2)+C));
uf = uf';

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for plane wave initial data:
t_0 = 0; t_f = 5; M = 10; N = 32; 
dt = (t_f-t_0)/M;
NPrint = M;
% choose focussing (K=1) or defocussing (K=-1):
K = 1;
dx = 2*pi/N;
x = 0:dx:2*pi;
A = 4;
B = 3; 
C = 0;
u0 = A*exp(i*(B*x+0*(-K*A^2+B^2)+C));
u0 = u0';
uf = A*exp(i*(B*x+t_f*(-K*A^2+B^2)+C));
uf = uf';

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

dt = dt/2;
NPrint = 2*NPrint;
[u7,x,t7,kk,amp7,M7,H7] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u8,x,t8,kk,amp8,M8,H8] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u9,x,t9,kk,amp9,M9,H9] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

figure(1)
clf
plot(kk,amp5(:,1),'o-');
hold on
plot(kk,amp5(:,11),'o-');
plot(kk,amp5(:,21),'o-');
plot(kk,amp5(:,31),'o-');
print -dps split_schroedinger_focussing_unstable_spec.ps

figure(2)
clf
plot(kk,amp6(:,1),'o-');
hold on
plot(kk,amp6(:,21),'o-');
plot(kk,amp6(:,41),'o-');
plot(kk,amp6(:,61),'o-');
print -dps split_schroedinger_focussing_stable_spec.ps

figure(3)
clf
plot(kk,amp6(:,61),'o-')
hold on
plot(kk,amp7(:,121),'o-')
plot(kk,amp8(:,241),'o-')
plot(kk,amp9(:,481),'o-')
print -dps split_schroeinger_ben_feir_spec.ps

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

N = 2048; 
dx = 2*pi/N;
x = 0:dx:2*pi;
alpha = 200;
U = 1;
x0 = pi;
phi0 = 0;
u0 = sqrt(2*alpha)*exp(i*(U/2*x - (alpha - U^2/4)*0 + phi0))./cosh(sqrt(alpha)*(x+U*0-x0));
u0 = u0';
uf = sqrt(2*alpha)*exp(i*(U/2*x - (alpha - U^2/4)*1 + phi0))./cosh(sqrt(alpha)*(x+U*1-x0));

v = fft(u0);
amp0(N/2:N-1,1) = log10( abs(v(1:N/2))/N + 1.e-16);
amp0(1:N/2-1,1) = log10( abs(v(N/2+2:N))/N + 1.e-16);
kk = (-N/2+1):1:(N/2-1);
v = fft(uf);
ampf(N/2:N-1,1) = log10( abs(v(1:N/2))/N + 1.e-16);
ampf(1:N/2-1,1) = log10( abs(v(N/2+2:N))/N + 1.e-16);

figure(1)
clf
plot(kk,amp0),
hold on
plot(kk,ampf),
axis([-1024,1024,-17,1])
print -dps exact_spectra.ps

t_0 = 0; t_f = 1; M = 2000;
dt = (t_f-t_0)/M;
NPrint = M;
[u1,x,t,kk,amp1,M1,H1] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);
% [u2,x,t,kk,amp2,M2,H2] = split_schroedinger2(u0,K,t_0,t_f,dt,NPrint,N);

figure(2)
clf
hold on
plot(kk,ampf),
plot(kk,amp1(:,length(t)))
% plot(kk,amp2(:,length(t)))
axis([-1024,1024,-17,1])
print -dps approx_sol_spectra.ps


figure(3)
clf
hold on
plot(x,abs(u0),'--')
plot(x,abs(uf))
axis([0,2*pi,0,21])
print -dps exact_sol.ps

figure(4)
clf
hold on
plot(x,abs(u0),'--')
plot(x,abs(uf))
plot(x,abs(u1(:,length(t))))
%plot(x,abs(u2(:,length(t))))
axis([0,2*pi,0,21])
print -dps approx_sol.ps




