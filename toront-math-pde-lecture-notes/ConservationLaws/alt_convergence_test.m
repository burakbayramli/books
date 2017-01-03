% Do convegence test before the shock time.  Lax-Wendroff, differences
% measured wrt L^1 and Richardson ratios
% t_0 = 0; t_f = .1; N = 100; M = 100;
% [u1,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u1 = u1(:,length(t));
% dx1 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u2,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u2 = u2(:,length(t));
% dx2 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u3,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u3 = u3(:,length(t));
% dx3 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u4,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u4 = u4(:,length(t));
% dx4 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u5,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u5 = u5(:,length(t));
% dx5 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u6,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u6 = u6(:,length(t));
% dx6 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u7,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u7 = u7(:,length(t));
% dx7 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u8,x,t] = conservative_scheme_big(@ID,@F_lw,t_0,t_f,M,N,1);
% dx8 = 2*pi/N;
%  
% save alt_short_time_LW.mat x t u* dx*

load alt_short_time_LW.mat

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

% computing up to time t=.1 with initial N=100, M=100
% rat = 
%    5.4049    3.6298    3.8928    3.9449    3.9830    3.9954
% D =
%  6.6303e-03   2.1674e-03   6.2798e-04   1.6173e-04   4.0980e-05   1.0276e-05   2.5719e-06
% D(1:6)./D(2:7)
% ans =
% 3.0591e+00   3.4514e+00   3.8828e+00   3.9466e+00   3.9880e+00   3.9954e+00

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   
% Do convegence test after the shock time.  Lax-Wendroff, differences
% measured wrt L^1 and Richardson ratios
% t_0 = 0; t_f = .6; N = 100; M = 600;
% [u1,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u1 = u1(:,length(t));
% dx1 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u2,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u2 = u2(:,length(t));
% dx2 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u3,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u3 = u3(:,length(t));
% dx3 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u4,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u4 = u4(:,length(t));
% dx4 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u5,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u5 = u5(:,length(t));
% dx5 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u6,x,t] = conservative_scheme(@ID,@F_lw,t_0,t_f,M,N,1);
% u6 = u6(:,length(t));
% dx6 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u7,x,t] = conservative_scheme_big(@ID,@F_lw,t_0,t_f,M,N,1);
% % u7 = u7(:,length(t));
% dx7 = 2*pi/N;
%  
% N = N*2; M = M*2
% [u8,x,t] = conservative_scheme_big(@ID,@F_lw,t_0,t_f,M,N,1);
% dx8 = 2*pi/N;
%  
% save alt_long_time_LW.mat x t u* dx*

load alt_long_time_LW.mat

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


% computing up to time t=.6 with initial N=100, M=100
% rat = 
% 1.2228   1.1466   1.1385   1.3624   1.5378   1.9148
%   
% D =
% 1.6376e-01 1.2609e-01 1.0674e-01 9.1827e-02 6.5023e-02 4.2144e-02   2.2009e-02
% D(1:6)./D(2:7)
% ans =
% 1.2987 1.1812 1.1624 1.4122 1.5429 1.9148
