%nm6p09_1.m
%y"-y'+y=3*e^2t-2sin(t) with y(0)=5 & y(2)=-10
t0=0; tf=2; y0=5; yf=-10; N=100; tol=1e-6; kmax=10;
df=inline('[y(2); y(2)-y(1)+3*exp(2*t)-2*sin(t)]','t','y');    
a1=-1; a0=1; u=inline('3*exp(2*t)-2*sin(t)','t'); 
solinit = bvpinit(linspace(t0,tf,5),[-10 5]); %[1 9]
fbc=inline('[y0(1)-5; yf(1)+10]','y0','yf');
% Shooting method
tic, [tt,y_sh]=bvp2_shoot(df,t0,tf,y0,yf,N,tol,kmax); times(1)=toc;
% Finite difference method
tic, [tt,y_fd]=bvp2_fdf(a1,a0,u,t0,tf,y0,yf,N); times(2)=toc;
% MATLAB built-in function bvp4c
sol=bvp4c(df,fbc,solinit,bvpset('RelTol',1e-6)); 
tic, y_bvp = deval(sol,tt); times(3)=toc
% Eror evaluation
ys=[y_sh(:,1) y_fd y_bvp(1,:)']; plot(tt,ys) 
err=err_of_sol_de(df,tt,ys)
