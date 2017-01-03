% for plane wave initial data:
% t_0 = 0; t_f = 5; M = 10; N = 32; 
t_0 = 0; t_f = .1; M = 10; N = 256; 
dt = (t_f-t_0)/M;
NPrint = M;
% choose focussing (K=1) or defocussing (K=-1):
K = -1;
% the scheme appears to be unstable for the focussing case --- I can't find
% timesteps small enough to not lead to blow up...
dx = 2*pi/N;
x = 0:dx:2*pi;
% A = 4;
% B = 3; 
% C = 0;
% u0 = A*exp(i*(B*x+0*(-K*A^2+B^2)+C));
% u0 = u0';
% uf = A*exp(i*(B*x+t_f*(-K*A^2+B^2)+C));
% uf = uf';
% initial data
u0 = exp(exp(i*x));
u0 = u0';


[u1,x,t1,kk,amp1,M1,H1] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u2,x,t2,kk,amp2,M2,H2] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u3,x,t3,kk,amp3,M3,H3] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u4,x,t4,kk,amp4,M4,H4] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u5,x,t5,kk,amp5,M5,H5] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u6,x,t6,kk,amp6,M6,H6] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

dt = dt/2;
NPrint = 2*NPrint;
[u7,x,t7,kk,amp7,M7,H7] = split_schroedinger3(u0,K,t_0,t_f,dt,NPrint,N);

% for initial data
%    u0 = A*exp(i*(B*x+0*(-K*A^2+B^2)+C));
% make plots of the instability
% figure(1)
% clf
% plot(kk,amp5(:,1),'o-');
% hold on
% plot(kk,amp5(:,11),'o-');
% plot(kk,amp5(:,21),'o-');
% plot(kk,amp5(:,31),'o-');
% print -dps split_schroedinger_unstable_spec.ps
% 
% figure(2)
% clf
% plot(kk,amp6(:,1),'o-');
% hold on
% plot(kk,amp6(:,21),'o-');
% plot(kk,amp6(:,41),'o-');
% plot(kk,amp6(:,61),'o-');
% print -dps split_schroedinger_stable_spec.ps


max(abs(u1(:,length(t1))-u2(:,length(t2))))
max(abs(u2(:,length(t2))-u3(:,length(t3))))
max(abs(u3(:,length(t3))-u4(:,length(t4))))
max(abs(u5(:,length(t5))-u4(:,length(t4))))
max(abs(u5(:,length(t5))-u6(:,length(t6))))
max(abs(u6(:,length(t6))-u7(:,length(t7))))

% max(abs(u1(:,length(t1))-uf))
% max(abs(u2(:,length(t2))-uf))
% max(abs(u3(:,length(t3))-uf))
% max(abs(u4(:,length(t4))-uf))
% max(abs(u5(:,length(t5))-uf))
% max(abs(u6(:,length(t6))-uf))
% max(abs(u7(:,length(t7))-uf))


rat1 = max(abs(u1(:,length(t1))-u2(:,length(t2))))/max(abs(u2(:,length(t2))-u3(:,length(t3))));
rat2 = max(abs(u2(:,length(t2))-u3(:,length(t3))))/max(abs(u3(:,length(t3))-u4(:,length(t4))));
rat3 = max(abs(u3(:,length(t3))-u4(:,length(t4))))/max(abs(u4(:,length(t4))-u5(:,length(t5))));
rat4 = max(abs(u5(:,length(t5))-u4(:,length(t4))))/max(abs(u6(:,length(t6))-u5(:,length(t5))));
rat5 = max(abs(u5(:,length(t5))-u6(:,length(t6))))/max(abs(u6(:,length(t6))-u7(:,length(t7))));

abs(M1(length(t1))-M1(1))
abs(M2(length(t2))-M2(1))
abs(M3(length(t3))-M3(1))
abs(M4(length(t4))-M4(1))
abs(M5(length(t5))-M5(1))
abs(M6(length(t6))-M6(1))
abs(M7(length(t7))-M7(1))


abs(M1(length(t1))-M1(1))/abs(M2(length(t2))-M2(1))
abs(M2(length(t2))-M2(1))/abs(M3(length(t3))-M3(1))
abs(M3(length(t3))-M3(1))/abs(M4(length(t4))-M4(1))
abs(M4(length(t4))-M4(1))/abs(M5(length(t5))-M5(1))
abs(M5(length(t5))-M5(1))/abs(M6(length(t6))-M6(1))
abs(M6(length(t6))-M6(1))/abs(M7(length(t7))-M7(1))


abs(H1(length(t1))-H1(1))
abs(H2(length(t2))-H2(1))
abs(H3(length(t3))-H3(1))
abs(H4(length(t4))-H4(1))
abs(H5(length(t5))-H5(1))
abs(H6(length(t6))-H6(1))
abs(H7(length(t7))-H7(1))



abs(H1(length(t1))-H1(1))/abs(H2(length(t2))-H2(1))
abs(H2(length(t2))-H2(1))/abs(H3(length(t3))-H3(1))
abs(H3(length(t3))-H3(1))/abs(H4(length(t4))-H4(1))
abs(H4(length(t4))-H4(1))/abs(H5(length(t5))-H5(1))
abs(H5(length(t5))-H5(1))/abs(H6(length(t6))-H6(1))
abs(H6(length(t6))-H6(1))/abs(H7(length(t7))-H7(1))
