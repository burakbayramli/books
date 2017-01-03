%nm651_1.m to solve a set of differential eq.s, i.e., state equation
clear
df= 'df651';  
%df=inline('[x(2) -x(2)+1]','t','x'); %row vector ftn
%df1=inline('[x(2); -x(2)+1]','t','x'); %column vector ftn
t0=0; tf=2; x0=[1 -1]; N=45;
[t1,xR]=ode_RK4(df,[t0 tf],x0,N);
%[t1,xM]=ode_Milne(df,[t0 tf],x0,N);
[t1,xA]=ode_ABM(df,[t0 tf],x0,N);
tic, [t1,xH]=ode_Ham(df,[t0 tf],x0,N); toc
%soptions=odeset('RelTol',1e-4);
tic, [t45,x45]=ode45(df,[t0 tf],x0); toc
%[t1,xG]=Gear(df,[t0 tf],x0,N);
%compare with the true analytical solution
x(:,1)=t1-1+2*exp(-t1); x(:,2)=1-2*exp(-t1);
err_Ham=norm(xH-x)/length(t1)
x4(:,1)=t45-1+2*exp(-t45); x4(:,2)=1-2*exp(-t45);
err_45=norm(x45-x4)/length(t45)