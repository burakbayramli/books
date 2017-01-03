N = 32;
NPrint = 10;
dt = 1/2;
h = 2*pi/N;
x=0:h:2*pi-h;
u0 = 4*exp(i*3*x);
% exact solution 4*exp(i*(3*x-7*t))
% defocussing case
K = -1;
t_0 = 0; 
t_f = 5;


[u1,x,t1,kk,amp1,M1,H1] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u2,x,t2,kk,amp2,M2,H2] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u3,x,t3,kk,amp3,M3,H3] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u4,x,t4,kk,amp4,M4,H4] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u5,x,t5,kk,amp5,M5,H5] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u6,x,t6,kk,amp6,M6,H6] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

for j=1:11
    plot(kk,amp1(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end
  
for j=1:11
    plot(kk,amp2(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end

for j=1:11
    plot(kk,amp3(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end

for j=1:11
    plot(kk,amp4(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end

for j=1:11
    plot(kk,amp5(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end

for j=1:11
    plot(kk,amp6(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end

N = 32;
NPrint = 32;
dt = 1/32;
h = 2*pi/N;
x=0:h:2*pi-h;
u0 = 4*exp(i*3*x);
% exact solution 4*exp(i*(3*x+25*t))
% focussing case
K = 1;
t_0 = 0; 
t_f = 1;


[u1,x,t1,kk,amp1,M1,H1] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u2,x,t2,kk,amp2,M2,H2] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u3,x,t3,kk,amp3,M3,H3] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u4,x,t4,kk,amp4,M4,H4] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u5,x,t5,kk,amp5,M5,H5] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u6,x,t6,kk,amp6,M6,H6] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
[u7,x,t7,kk,amp7,M7,H7] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);


for j=1:NPrint+1
    plot(kk,amp1(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end
  
for j=1:NPrint+1
    plot(kk,amp2(:,j),'o-');
    axis([min(kk),max(kk),-16,2]);
    figure(1)
    pause(1)
end

figure(1)
clf
plot(kk,amp1(:,NPrint+1))
hold on
plot(kk,amp2(:,NPrint+1))
plot(kk,amp3(:,NPrint+1))
plot(kk,amp4(:,NPrint+1))
plot(kk,amp5(:,NPrint+1))



N = 4096;
h = 2*pi/N;
x=0:h:2*pi-h;
t = 0;
u0 = sqrt(400)*exp(i*(x/2+(40000-1/4)*t))./cosh(sqrt(200)*(x-t-pi));
v = fft(u0);
clear amp
amp(N/2:N-1) = log10( abs(v(1:N/2)) + 1.e-16);
amp(1:N/2-1) = log10( abs(v(N/2+2:N)) + 1.e-16);
amp0 = amp;
t = 1;
uf = sqrt(400)*exp(i*(x/2+(40000-1/4)*t))./cosh(sqrt(200)*(x-t-pi));
v = fft(uf);
amp(N/2:N-1) = log10( abs(v(1:N/2)) + 1.e-16);
amp(1:N/2-1) = log10( abs(v(N/2+2:N)) + 1.e-16);
ampf = amp;

dt = 1/2000;
t_0 = 0;
t_f = 1;
NPrint = 100;
% focussing case
K = 1;

[u,xx,t,kk,amp,M,H] = split_schroedinger1(u0,K,t_0,t_f,dt,NPrint,N);

figure(1)
clf
plot(kk,amp0);
hold on
plot(kk,ampf);
axis([min(kk),max(kk),-16,4]);
figure(1)

figure(2)
clf
plot(x,abs(u0));
hold on
plot(x,abs(uf),'r')
figure(2)

figure(3)
clf
plot(kk,amp(:,1));
hold on
plot(kk,amp(:,101),'r');
axis([min(kk),max(kk),-16,4]);
figure(3)


figure(4)
clf
plot(xx,abs(u(:,1)));
hold on
plot(xx,abs(u(:,101)),'r')
figure(4)

[u,xx,t,kk,amp,M,H] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

figure(5)
clf
plot(kk,amp(:,1));
hold on
plot(kk,amp(:,101),'r');
axis([min(kk),max(kk),-16,4]);
figure(5)


figure(6)
clf
plot(xx,abs(u(:,1)));
hold on
plot(xx,abs(u(:,101)),'r')
figure(6)
