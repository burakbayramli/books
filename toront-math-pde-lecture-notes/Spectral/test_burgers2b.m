t_0 = 0; t_f = .01; M = 10; N = 128; L = 2*pi;
% I'm getting convergence when L = 2*pi but not otherwise.  I need to 
% debug burgers2.m

D = 2;

dx = L/N;
x = 0:dx:L-dx;
% if phi is a solution of
%                  phi_t = D*phi_xx
% then u = -2*D*phi_x/phi is a solution of
%                  u_t + u u_x = D u_xx
%
% I take phi = phi0+phi1+phi2 where
%    phi0 = 3;
%    phi1 = exp(-D*1^2*(2*pi/L)^2*t)*sin(1*2*pi*x/L)
%    phi2 = exp(-D*2^2*(2*pi/L)^2*t)*sin(2*2*pi*x/L)   
%
% This results in initial data for u:
%
%
l1 = 1;
l2 = 2;
%
u0 = -4*D*cos(2*pi*x/L)*pi./(L*(3+sin(2*pi*x/L)));
%
% and solution at time t_f (assuming t_0 = 0) of
%
uf = -4*D*exp(-4*D*pi^2*t_f/L^2)*cos(2*pi*x/L)*pi./(L*(3+exp(-4*D*pi^2*t_f/L^2)*sin(2*pi*x/L)));
uf = uf';

[u1,x,t1,kk,amp1] = burgers2b(u0,D,t_0,t_f,M,N);
err(1) = max(abs(u1(:,M+1)-uf));

M = 2*M;
[u2,x,t2,kk,amp2] = burgers2b(u0,D,t_0,t_f,M,N);
err(2) = max(abs(u2(:,M+1)-uf));

M = 2*M;
[u3,x,t3,kk,amp3] = burgers2b(u0,D,t_0,t_f,M,N);
err(3) = max(abs(u3(:,M+1)-uf));

M = 2*M;
[u4,x,t4,kk,amp4] = burgers2b(u0,D,t_0,t_f,M,N);
err(4) = max(abs(u4(:,M+1)-uf));

M = 2*M;
[u5,x,t5,kk,amp5] = burgers2b(u0,D,t_0,t_f,M,N);
err(5) = max(abs(u5(:,M+1)-uf));

M = 2*M;
[u6,x,t6,kk,amp6] = burgers2b(u0,D,t_0,t_f,M,N);
err(6) = max(abs(u6(:,M+1)-uf));

M = 2*M;
[u7,x,t7,kk,amp7] = burgers2b(u0,D,t_0,t_f,M,N);
err(7) = max(abs(u7(:,M+1)-uf));

rat1 = max(abs(u1(:,length(t1))-u2(:,length(t2))))/max(abs(u2(:,length(t2))-u3(:,length(t3))));
rat2 = max(abs(u2(:,length(t2))-u3(:,length(t3))))/max(abs(u3(:,length(t3))-u4(:,length(t4))));
rat3 = max(abs(u3(:,length(t3))-u4(:,length(t4))))/max(abs(u4(:,length(t4))-u5(:,length(t5))));
rat4 = max(abs(u5(:,length(t5))-u4(:,length(t4))))/max(abs(u6(:,length(t6))-u5(:,length(t5))));
rat5 = max(abs(u5(:,length(t5))-u6(:,length(t6))))/max(abs(u6(:,length(t6))-u7(:,length(t7))));

err
err(1:6)./err(2:7)
