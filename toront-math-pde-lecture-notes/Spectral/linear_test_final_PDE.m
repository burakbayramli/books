
t_0 = 0; t_f = .000001; M = 20; N = 64;

dx = 2*pi/N;
x = 0:dx:2*pi-dx;

eps = .1;
ubar = 2;
k = 3;
u0 = ubar + eps*cos(k*x);

[u1,x,t1,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
M = 2*M;
[u2,x,t2,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
M = 2*M;
[u3,x,t3,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
M = 2*M;
[u4,x,t4,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
M = 2*M;
[u5,x,t5,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
M = 2*M;
[u6,x,t6,kk,amp] = final_PDE(u0,t_0,t_f,M,N);

format short e
format compact
max(abs(u1(:,length(t1))-u2(:,length(t2))))
max(abs(u2(:,length(t2))-u3(:,length(t3))))
max(abs(u3(:,length(t3))-u4(:,length(t4))))
max(abs(u4(:,length(t3))-u5(:,length(t4))))
max(abs(u5(:,length(t3))-u6(:,length(t4))))

max(abs(u1(:,length(t1))-u2(:,length(t2))))/max(abs(u2(:,length(t2))-u3(:,length(t3))))

max(abs(u2(:,length(t2))-u3(:,length(t3))))/max(abs(u3(:,length(t3))-u4(:,length(t4))))

max(abs(u3(:,length(t3))-u4(:,length(t4))))/max(abs(u4(:,length(t4))-u5(:,length(t5))))
max(abs(u4(:,length(t4))-u5(:,length(t5))))/max(abs(u5(:,length(t5))-u6(:,length(t6))))

% we've got our time-step small enough.  Now that M is fixed, play around
% with epsilon and the exact solution of the linearized problem.

M = 80;
eps = 1;
ubar = 2;
k = 3;
u0 = ubar + eps*cos(k*x);
[u1,x,t,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
for j=1:M+1
  u1exact(:,j) = ubar + eps*exp((-k^2+10*abs(k))*t(j))*cos(k*x+ubar^3*k*t(j)/100);
end

eps = eps/2;
u0 = ubar + eps*cos(k*x);
[u2,x,t,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
for j=1:M+1
  u2exact(:,j) = ubar + eps*exp((-k^2+10*abs(k))*t(j))*cos(k*x+ubar^3*k*t(j)/100);
end

eps = eps/2;
u0 = ubar + eps*cos(k*x);
[u3,x,t,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
for j=1:M+1
  u3exact(:,j) = ubar + eps*exp((-k^2+10*abs(k))*t(j))*cos(k*x+ubar^3*k*t(j)/100);
end

eps = eps/2;
u0 = ubar + eps*cos(k*x);
[u4,x,t,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
for j=1:M+1
  u4exact(:,j) = ubar + eps*exp((-k^2+10*abs(k))*t(j))*cos(k*x+ubar^3*k*t(j)/100);
end

eps = eps/2;
u0 = ubar + eps*cos(k*x);
[u5,x,t,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
for j=1:M+1
  u5exact(:,j) = ubar + eps*exp((-k^2+10*abs(k))*t(j))*cos(k*x+ubar^3*k*t(j)/100);
end

eps = eps/2;
u0 = ubar + eps*cos(k*x);
[u6,x,t,kk,amp] = final_PDE(u0,t_0,t_f,M,N);
for j=1:M+1
  u6exact(:,j) = ubar + eps*exp((-k^2+10*abs(k))*t(j))*cos(k*x+ubar^3*k*t(j)/100);
end

figure(1)
clf
plot(x,log10(abs(u1(:,M+1)-u1exact(:,M+1))))
hold on
plot(x,log10(abs(u2(:,M+1)-u2exact(:,M+1))),'r')
plot(x,log10(abs(u3(:,M+1)-u3exact(:,M+1))),'g')
plot(x,log10(abs(u4(:,M+1)-u4exact(:,M+1))),'w')
plot(x,log10(abs(u5(:,M+1)-u5exact(:,M+1))),'c')
plot(x,log10(abs(u6(:,M+1)-u6exact(:,M+1))))

e1 = max(abs(u1(:,M+1)-u1exact(:,M+1)))
e2 = max(abs(u2(:,M+1)-u2exact(:,M+1)))
e3 = max(abs(u3(:,M+1)-u3exact(:,M+1)))
e4 = max(abs(u4(:,M+1)-u4exact(:,M+1)))
e5 = max(abs(u5(:,M+1)-u5exact(:,M+1)))
e6 = max(abs(u6(:,M+1)-u6exact(:,M+1)))

e1/e2
e2/e3
e3/e4
e4/e5
e5/e6


