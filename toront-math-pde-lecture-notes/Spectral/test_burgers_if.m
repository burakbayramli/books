t_0 = 0; t_f = .01; M = 10; N = 128;
D = 2;

dx = 2*pi/N;
x = 0:dx:2*pi-dx;
u0 = -2*D*cos(x)./(3+sin(x));
M = 10;
t_0 = 0; 
t_f = 1/100;

u_exact = -2*D*exp(-D*t_f)*cos(x)./(3+exp(-D*t_f)*sin(x));
u_exact = u_exact';

M
[u1,x,t1,kk,amp1] = burgers_if(u0,D,t_0,t_f,M,N);
err(1) = max(abs(u1(:,M+1)-u_exact));

M = 2*M
[u2,x,t2,kk,amp2] = burgers_if(u0,D,t_0,t_f,M,N);
err(2) = max(abs(u2(:,M+1)-u_exact));

M = 2*M
[u3,x,t3,kk,amp3] = burgers_if(u0,D,t_0,t_f,M,N);
err(3) = max(abs(u3(:,M+1)-u_exact));

M = 2*M
[u4,x,t4,kk,amp4] = burgers_if(u0,D,t_0,t_f,M,N);
err(4) = max(abs(u4(:,M+1)-u_exact));

M = 2*M
[u5,x,t5,kk,amp5] = burgers_if(u0,D,t_0,t_f,M,N);
err(5) = max(abs(u5(:,M+1)-u_exact));

M = 2*M
[u6,x,t6,kk,amp6] = burgers_if(u0,D,t_0,t_f,M,N);
err(6) = max(abs(u6(:,M+1)-u_exact));

M = 2*M
[u7,x,t7,kk,amp7] = burgers_if(u0,D,t_0,t_f,M,N);
err(7) = max(abs(u7(:,M+1)-u_exact));

rat1 = max(abs(u1(:,length(t1))-u2(:,length(t2))))/max(abs(u2(:,length(t2))-u3(:,length(t3))));
rat2 = max(abs(u2(:,length(t2))-u3(:,length(t3))))/max(abs(u3(:,length(t3))-u4(:,length(t4))));
rat3 = max(abs(u3(:,length(t3))-u4(:,length(t4))))/max(abs(u4(:,length(t4))-u5(:,length(t5))));
rat4 = max(abs(u5(:,length(t5))-u4(:,length(t4))))/max(abs(u6(:,length(t6))-u5(:,length(t5))));
rat5 = max(abs(u5(:,length(t5))-u6(:,length(t6))))/max(abs(u6(:,length(t6))-u7(:,length(t7))));

err
err(1:6)./err(2:7)
