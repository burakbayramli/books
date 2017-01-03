
% Do convegence test before the shock time.  Explicit upwind, differences
% measured wrt L^1 and Richardson ratios
% t_0 = 0; t_f = .03; N = 10; M = 10;
% [u1,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx1 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u2,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx2 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u3,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx3 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u4,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx4 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u5,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx5 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u6,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx6 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u7,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx7 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u8,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx8 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u9,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% dx9 = 2*pi/N;
% 
% save short_time_EU.mat x t u* dx*

load short_time_EU.mat

diff1 = (u1(:,11)-u2(1:2:21,21));
diff2 = (u2(1:2:21,21)-u3(1:4:41,41));
diff1 = dx1*sum(abs(diff1));
diff2 = dx1*sum(abs(diff2));
rat(1) = diff1./diff2;

diff1 = (u2(:,21)-u3(1:2:41,41));
diff2 = (u3(1:2:41,41)-u4(1:4:81,81));
diff1 = dx2*sum(abs(diff1));
diff2 = dx2*sum(abs(diff2));
rat(2) = diff1./diff2;

diff1 = (u3(:,41)-u4(1:2:81,81));
diff2 = (u4(1:2:81,81)-u5(1:4:161,161));
diff1 = dx3*sum(abs(diff1));
diff2 = dx3*sum(abs(diff2));
rat(3) = diff1./diff2;

diff1 = (u4(:,81)-u5(1:2:161,161));
diff2 = (u5(1:2:161,161)-u6(1:4:321,321));
diff1 = dx4*sum(abs(diff1));
diff2 = dx4*sum(abs(diff2));
rat(4) = diff1./diff2;

diff1 = (u5(:,161)-u6(1:2:321,321));
diff2 = (u6(1:2:321,321)-u7(1:4:641,641));
diff1 = dx5*sum(abs(diff1));
diff2 = dx5*sum(abs(diff2));
rat(5) = diff1./diff2;

diff1 = (u6(:,321)-u7(1:2:641,641));
diff2 = (u7(1:2:641,641)-u8(1:4:1281,1281));
diff1 = dx6*sum(abs(diff1));
diff2 = dx6*sum(abs(diff2));
rat(6) = diff1./diff2;

diff1 = (u7(:,641)-u8(1:2:1281,1281));
diff2 = (u8(1:2:1281,1281)-u9(1:4:2561,2561));
diff1 = dx7*sum(abs(diff1));
diff2 = dx7*sum(abs(diff2));
rat(7) = diff1./diff2
% computing up to time t=.3 with initial N=10 and M=10
% rat =
%    2.1846   1.5636   1.7459   1.8433   1.8963   1.9448  1.9690
% computing up to time t=.03 with initial N=10 and M=10
% rat =
%    3.9731   2.2632   1.8873   2.0075   1.9910   1.9955   1.9977
   
% Do convegence test before the shock time.  Lax-Friedrichs, differences
% measured wrt L^1 and Richardson ratios
% t_0 = 0; t_f = .03; N = 10; M = 10;
% [u1,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx1 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u2,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx2 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u3,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx3 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u4,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx4 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u5,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx5 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u6,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx6 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u7,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx7 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u8,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx8 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u9,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% dx9 = 2*pi/N;
% 
% save short_time_LF.mat x t u* dx*

load short_time_LF.mat

diff1 = (u1(:,11)-u2(1:2:21,21));
diff2 = (u2(1:2:21,21)-u3(1:4:41,41));
diff1 = dx1*sum(abs(diff1));
diff2 = dx1*sum(abs(diff2));
rat(1) = diff1./diff2;

diff1 = (u2(:,21)-u3(1:2:41,41));
diff2 = (u3(1:2:41,41)-u4(1:4:81,81));
diff1 = dx2*sum(abs(diff1));
diff2 = dx2*sum(abs(diff2));
rat(2) = diff1./diff2;

diff1 = (u3(:,41)-u4(1:2:81,81));
diff2 = (u4(1:2:81,81)-u5(1:4:161,161));
diff1 = dx3*sum(abs(diff1));
diff2 = dx3*sum(abs(diff2));
rat(3) = diff1./diff2;

diff1 = (u4(:,81)-u5(1:2:161,161));
diff2 = (u5(1:2:161,161)-u6(1:4:321,321));
diff1 = dx4*sum(abs(diff1));
diff2 = dx4*sum(abs(diff2));
rat(4) = diff1./diff2;

diff1 = (u5(:,161)-u6(1:2:321,321));
diff2 = (u6(1:2:321,321)-u7(1:4:641,641));
diff1 = dx5*sum(abs(diff1));
diff2 = dx5*sum(abs(diff2));
rat(5) = diff1./diff2;

diff1 = (u6(:,321)-u7(1:2:641,641));
diff2 = (u7(1:2:641,641)-u8(1:4:1281,1281));
diff1 = dx6*sum(abs(diff1));
diff2 = dx6*sum(abs(diff2));
rat(6) = diff1./diff2;

diff1 = (u7(:,641)-u8(1:2:1281,1281));
diff2 = (u8(1:2:1281,1281)-u9(1:4:2561,2561));
diff1 = dx7*sum(abs(diff1));
diff2 = dx7*sum(abs(diff2));
rat(7) = diff1./diff2
% computing up to t=.3 with initial N=10, M=10
% rat =
%    1.0156   1.3589   1.4734   1.5131   1.5640   1.6299   1.7056
% computing up to t=.03 with initial N=10, M=10
% rat =
%    9.9884e-01   1.3587   1.4829   1.5374   1.6056   1.6894   1.7767
   
% Do convegence test before the shock time.  Lax-Wendroff, differences
% measured wrt L^1 and Richardson ratios
% t_0 = 0; t_f = .03; N = 10; M = 10;
% [u1,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx1 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u2,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx2 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u3,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx3 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u4,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx4 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u5,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx5 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u6,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx6 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u7,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx7 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u8,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx8 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u9,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% dx9 = 2*pi/N;
% 
% save short_time_LW.mat x t u* dx*

load short_time_LW.mat

diff1 = (u1(:,11)-u2(1:2:21,21));
diff2 = (u2(1:2:21,21)-u3(1:4:41,41));
diff1 = dx1*sum(abs(diff1));
diff2 = dx1*sum(abs(diff2));
rat(1) = diff1./diff2;
D(1) = diff1;

diff1 = (u2(:,21)-u3(1:2:41,41));
diff2 = (u3(1:2:41,41)-u4(1:4:81,81));
diff1 = dx2*sum(abs(diff1));
diff2 = dx2*sum(abs(diff2));
rat(2) = diff1./diff2;
D(2) = diff1;

diff1 = (u3(:,41)-u4(1:2:81,81));
diff2 = (u4(1:2:81,81)-u5(1:4:161,161));
diff1 = dx3*sum(abs(diff1));
diff2 = dx3*sum(abs(diff2));
rat(3) = diff1./diff2;
D(3) = diff1;

diff1 = (u4(:,81)-u5(1:2:161,161));
diff2 = (u5(1:2:161,161)-u6(1:4:321,321));
diff1 = dx4*sum(abs(diff1));
diff2 = dx4*sum(abs(diff2));
rat(4) = diff1./diff2;
D(4) = diff1;

diff1 = (u5(:,161)-u6(1:2:321,321));
diff2 = (u6(1:2:321,321)-u7(1:4:641,641));
diff1 = dx5*sum(abs(diff1));
diff2 = dx5*sum(abs(diff2));
rat(5) = diff1./diff2;
D(5) = diff1;

diff1 = (u6(:,321)-u7(1:2:641,641));
diff2 = (u7(1:2:641,641)-u8(1:4:1281,1281));
diff1 = dx6*sum(abs(diff1));
diff2 = dx6*sum(abs(diff2));
rat(6) = diff1./diff2;
D(6) = diff1;

diff1 = (u7(:,641)-u8(1:2:1281,1281));
diff2 = (u8(1:2:1281,1281)-u9(1:4:2561,2561));
diff1 = dx7*sum(abs(diff1));
diff2 = dx7*sum(abs(diff2));
D(7) = diff1;
D(8) = diff2;
rat(7) = diff1./diff2
% computing up to time t=.3 with initial N=10, M=10
% rat =
%    2.2036   2.5455   3.9560   3.8584   3.9288   3.9731   3.9919
% computing up to time t=.03 with initial N=10, M=10
% rat =
%    2.3484   3.3376   3.6828   3.9412   3.9847   3.9958   3.9988
% D =
% 8.0421e-03 2.7696e-03 8.9641e-04 2.3327e-04 5.9272e-05 1.4891e-05 3.7240e-06 9.3128e-07
% D(1:7)./D(2:8)
% ans =
% 2.9037   3.0897  3.8428  3.9356  3.9805  3.9986  3.9988
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Do convegence test after the shock time.  Explicit upwind, differences
% measured wrt L^1 and Richardson ratios
% t_0 = 0; t_f = 2.5; N = 10; M = 10;
% t_0 = 0; t_f = 2.5; N = 100; M = 100;
% [u1,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u1 = u1(:,length(t));
% dx1 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u2,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u2 = u2(:,length(t));
% dx2 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u3,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u3 = u3(:,length(t));
% dx3 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u4,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u4 = u4(:,length(t));
% dx4 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u5,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u5 = u5(:,length(t));
% dx5 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u6,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u6 = u6(:,length(t));
% dx6 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u7,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u7 = u7(:,length(t));
% dx7 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u8,x,t] = conservative_scheme(@ID,@F_eu,t_0,t_f,M,N,1);
% u8 = u8(:,length(t));
% dx8 = 2*pi/N;
% 
% save long_time_EU.mat x t u* dx*

load long_time_EU.mat

diff1 = (u1-u2(1:2:201));
diff2 = (u2(1:2:201)-u3(1:4:401));
diff1 = dx1*sum(abs(diff1));
diff2 = dx1*sum(abs(diff2));
rat(1) = diff1./diff2;

diff1 = (u2-u3(1:2:401));
diff2 = (u3(1:2:401)-u4(1:4:801));
diff1 = dx2*sum(abs(diff1));
diff2 = dx2*sum(abs(diff2));
rat(2) = diff1./diff2;

diff1 = (u3-u4(1:2:801));
diff2 = (u4(1:2:801)-u5(1:4:1601));
diff1 = dx3*sum(abs(diff1));
diff2 = dx3*sum(abs(diff2));
rat(3) = diff1./diff2;

diff1 = (u4-u5(1:2:1601));
diff2 = (u5(1:2:1601)-u6(1:4:3201));
diff1 = dx4*sum(abs(diff1));
diff2 = dx4*sum(abs(diff2));
rat(4) = diff1./diff2;

diff1 = (u5(:)-u6(1:2:3201));
diff2 = (u6(1:2:3201)-u7(1:4:6401));
diff1 = dx5*sum(abs(diff1));
diff2 = dx5*sum(abs(diff2));
rat(5) = diff1./diff2;

diff1 = (u6-u7(1:2:6401));
diff2 = (u7(1:2:6401)-u8(1:4:12801));
diff1 = dx6*sum(abs(diff1));
diff2 = dx6*sum(abs(diff2));
rat(6) = diff1./diff2;
% computing up to time t=2.5 with the initial N = 10 and M = 10:
% rat =
%    5.9550   4.0204   2.8475   1.7883   1.3143   1.6881   2.3337
% computing up to time t=2.5 with the initial N = 100 and M = 100:
% rat =
%    1.5688   1.3056   2.1572   1.9168   2.2783   1.9130


% Do convegence test after the shock time.  Lax-Friedrichs, differences
% measured wrt L^1 and Richardson ratios
% t_0 = 0; t_f = 2.5; N = 10; M = 10;
% t_0 = 0; t_f = 2.5; N = 100; M = 100;
% [u1,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u1 = u1(:,length(t));
% dx1 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u2,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u2 = u2(:,length(t));
% dx2 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u3,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u3 = u3(:,length(t));
% dx3 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u4,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u4 = u4(:,length(t));
% dx4 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u5,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u5 = u5(:,length(t));
% dx5 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u6,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u6 = u6(:,length(t));
% dx6 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u7,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u7 = u7(:,length(t));
% dx7 = 2*pi/N;
% 
% N = N*2; M = M*2;
% [u8,x,t] = conservative_scheme(@ID,@F_lf,t_0,t_f,M,N,1);
% u8 = u8(:,length(t));
% dx8 = 2*pi/N;
% 
% save long_time_LF.mat x t u* dx*

load long_time_LF.mat
diff1 = (u1-u2(1:2:201));
diff2 = (u2(1:2:201)-u3(1:4:401));
diff1 = dx1*sum(abs(diff1));
diff2 = dx1*sum(abs(diff2));
rat(1) = diff1./diff2;

diff1 = (u2-u3(1:2:401));
diff2 = (u3(1:2:401)-u4(1:4:801));
diff1 = dx2*sum(abs(diff1));
diff2 = dx2*sum(abs(diff2));
rat(2) = diff1./diff2;

diff1 = (u3-u4(1:2:801));
diff2 = (u4(1:2:801)-u5(1:4:1601));
diff1 = dx3*sum(abs(diff1));
diff2 = dx3*sum(abs(diff2));
rat(3) = diff1./diff2;

diff1 = (u4-u5(1:2:1601));
diff2 = (u5(1:2:1601)-u6(1:4:3201));
diff1 = dx4*sum(abs(diff1));
diff2 = dx4*sum(abs(diff2));
rat(4) = diff1./diff2;

diff1 = (u5(:)-u6(1:2:3201));
diff2 = (u6(1:2:3201)-u7(1:4:6401));
diff1 = dx5*sum(abs(diff1));
diff2 = dx5*sum(abs(diff2));
rat(5) = diff1./diff2;

diff1 = (u6-u7(1:2:6401));
diff2 = (u7(1:2:6401)-u8(1:4:12801));
diff1 = dx6*sum(abs(diff1));
diff2 = dx6*sum(abs(diff2));
rat(6) = diff1./diff2;
% computing up to time t=2.5 with the initial N = 10 and M = 10:
% rat =
%    1.1620   1.4550   1.6523   1.8458   2.0217   2.0136   1.9900
% computing up to time t=2.5 with the initial N = 100 and M = 100:
% rat =
%    1.9150   2.0381   1.9948   2.0222   1.9845   1.9736
   
% Do convegence test after the shock time.  Lax-Wendroff, differences
% measured wrt L^1 and Richardson ratios
t_0 = 0; t_f = 2.5; N = 100; M = 100;
% [u1,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u1 = u1(:,length(t));
% dx1 = 2*pi/N;
% 
N = N*2; M = M*2;
% [u2,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u2 = u2(:,length(t));
% dx2 = 2*pi/N;
% 
N = N*2; M = M*2;
% [u3,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u3 = u3(:,length(t));
% dx3 = 2*pi/N;
% 
N = N*2; M = M*2;
% [u4,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u4 = u4(:,length(t));
% dx4 = 2*pi/N;
% 
N = N*2; M = M*2;
% [u5,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u5 = u5(:,length(t));
% dx5 = 2*pi/N;
% 
N = N*2; M = M*2;
% [u6,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u6 = u6(:,length(t));
% dx6 = 2*pi/N;
% 
N = N*2; M = M*2;
% [u7,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u7 = u7(:,length(t));
% dx7 = 2*pi/N;
% 
N = N*2; M = M*2;
% [u8,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u8 = u8(:,length(t));
% dx8 = 2*pi/N;

N = N*2; M = M*2;
[u9,x,t] = conservative_scheme_big(@ID,@F_lw,t_0,t_f,M,N,1);
u9 = u9(:,length(t));
dx9 = 2*pi/N;
% 
% save long_time_LW.mat x t u* dx*

load long_time_LW.mat

diff1 = (u1-u2(1:2:201));
diff2 = (u2(1:2:201)-u3(1:4:401));
diff1 = dx1*sum(abs(diff1))
D(1) = diff1;
diff2 = dx1*sum(abs(diff2))
rat(1) = diff1./diff2;

diff1 = (u2-u3(1:2:401));
diff2 = (u3(1:2:401)-u4(1:4:801));
diff1 = dx2*sum(abs(diff1))
D(2) = diff1;
diff2 = dx2*sum(abs(diff2))
rat(2) = diff1./diff2;

diff1 = (u3-u4(1:2:801));
diff2 = (u4(1:2:801)-u5(1:4:1601));
diff1 = dx3*sum(abs(diff1))
D(3) = diff1;
diff2 = dx3*sum(abs(diff2))
rat(3) = diff1./diff2;

diff1 = (u4-u5(1:2:1601));
diff2 = (u5(1:2:1601)-u6(1:4:3201));
diff1 = dx4*sum(abs(diff1))
D(4) = diff1;
diff2 = dx4*sum(abs(diff2))
rat(4) = diff1./diff2;

diff1 = (u5(:)-u6(1:2:3201));
diff2 = (u6(1:2:3201)-u7(1:4:6401));
diff1 = dx5*sum(abs(diff1))
diff2 = dx5*sum(abs(diff2))
D(5) = diff1;
rat(5) = diff1./diff2;

diff1 = (u6-u7(1:2:6401));
diff2 = (u7(1:2:6401)-u8(1:4:12801));
diff1 = dx6*sum(abs(diff1))
diff2 = dx6*sum(abs(diff2))
D(6) = diff1;
D(7) = diff2;
rat(6) = diff1./diff2;
% computing up to time t=2.5 with the initial N = 10 and M = 10:
% rat =
%    1.2942   1.7923   1.6329   1.3188   1.4115   2.0349 1.8264
% computing up to time t=2.5 with the initial N = 100 and M = 100:
% rat =
%    1.3217   1.5261   2.7588   1.3641   1.7232   2.3381


