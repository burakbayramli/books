%%% Finite Differences Theta Scheme for Parabolic Equation

T=2; %% final time
xmin=-10; xmax=10; %% spatial interval
L=xmax-xmin;

lambda=sqrt(5)/10;
theta=0.5-1/(12*lambda);
%theta=0.2;



%%% Calculations with different precisions.
t0=clock;
%%% First calculation
Nx=75; %% number of spatial discretization intervals
h=L/Nx; x=xmin:h:xmax;
u=InitCond(Nx,x);%figure(1); plot(x,u); axis([-10 10 -1 1]); drawnow;%pause(1)

dT=lambda*h^2; Nt=floor(T/dT); T=Nt*dT ;  %%% Reset time

u1=Fct_FD_ThetaScheme_ParabEqu(u,x,h,T,lambda,theta);

%%% Second calculation
Nx=2*Nx; %% number of spatial discretization intervals
h=L/Nx; x=xmin:h:xmax;

u=InitCond(Nx,x);%figure(1); plot(x,u); axis([-10 10 -1 1]); drawnow;%pause(1)

u2=Fct_FD_ThetaScheme_ParabEqu(u,x,h,T,lambda,theta);
u21=u2(1:2:Nx+1);

%%% Third calculation
Nx=2*Nx; %% number of spatial discretization intervals
h=L/Nx; x=xmin:h:xmax;

u=InitCond(Nx,x);%figure(1); plot(x,u); axis([-10 10 -1 1]); drawnow;%pause(1)

u3=Fct_FD_ThetaScheme_ParabEqu(u,x,h,T,lambda,theta); u31=u3(1:4:Nx+1);

etime(clock,t0)
%%% Speed of convergence (in norm L^2)
radius=norm(u21-u1)/norm(u31-u21)
order=log(radius)/log(2)
%norm(u31-u1)/norm(u31-u21)