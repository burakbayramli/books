clear
t_0 = 0; t_f = 1; N = 10; M = 10;

[u1,u1_exact,xi1,x1,t1] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;
[u2,u2_exact,xi2,x2,t2] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;
[u3,u3_exact,xi3,x3,t3] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;
[u4,u4_exact,xi4,x4,t4] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;
[u5,u5_exact,xi5,x5,t5] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;
[u6,u6_exact,xi6,x6,t6] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;
[u7,u7_exact,xi7,x7,t7] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;
[u8,u8_exact,xi8,x8,t8] = explicit_upwind(t_0,t_f,M,N);
N = 2*N; M = 2*M;

slope1 = mean(diff(xi1)./diff(t1))
slope2 = mean(diff(xi2)./diff(t2))
slope3 = mean(diff(xi3)./diff(t3))
slope4 = mean(diff(xi4)./diff(t4))
slope5 = mean(diff(xi5)./diff(t5))
slope6 = mean(diff(xi6)./diff(t6))
slope7 = mean(diff(xi7)./diff(t7))
slope8 = mean(diff(xi8)./diff(t8))

(slope1-slope2)/(slope2-slope3)
(slope2-slope3)/(slope3-slope4)
(slope3-slope4)/(slope4-slope5)
(slope4-slope5)/(slope5-slope6)
(slope5-slope6)/(slope6-slope7)
(slope6-slope7)/(slope7-slope8)

M=length(t6);
figure(1)
clf
plot(x6,u6(:,M),'LineWidth',2);
hold on
plot(x6,u6_exact(:,M),'--','LineWidth',2);
figure(1)
t6(M)
print -dps wrong_speed.ps

figure(2)
plot(x1,u1(:,length(t1)),'LineWidth',2);
hold on
plot(x2,u2(:,length(t2)),'LineWidth',2);
plot(x3,u3(:,length(t3)),'LineWidth',2);
plot(x4,u4(:,length(t4)),'LineWidth',2);
plot(x5,u5(:,length(t5)),'LineWidth',2);
plot(x6,u6(:,length(t6)),'LineWidth',2);
plot(x7,u7(:,length(t7)),'LineWidth',2);
plot(x8,u8(:,length(t8)),'LineWidth',2);
plot(x6,u6_exact(:,M),'--','LineWidth',2);




    


