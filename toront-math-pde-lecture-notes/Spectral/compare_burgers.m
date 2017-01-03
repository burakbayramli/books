N = 128;
dx = 2*pi/N;
x = 0:dx:2*pi-dx;

D = 2;
u0 = -2*D*cos(x)./(3+sin(x));

M = 640;
t_0 = 0; 
t_f = 1/100;

u_exact = -2*D*exp(-D*t_f)*cos(x)./(3+exp(-D*t_f)*sin(x));

[u_if,x,t,kk,amp] = burgers_if(u0,D,t_0,t_f,M,N);
[u_ee,x,t,kk,amp] = burgers_ee(u0,D,t_0,t_f,M,N);
[u_ab,x,t,kk,amp] = burgers_ab(u0,D,t_0,t_f,M,N);
[u_ie,x,t,kk,amp] = burgers_ie(u0,D,t_0,t_f,M,N);

max(abs(u_if(:,M+1)-u_exact'))
max(abs(u_ee(:,M+1)-u_exact'))
max(abs(u_ie(:,M+1)-u_exact'))
max(abs(u_ab(:,M+1)-u_exact'))