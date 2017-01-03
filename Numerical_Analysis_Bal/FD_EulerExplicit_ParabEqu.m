%%% Finite Differences Euler Explicit for Parabolic Equation

T=2; %% final time
xmin=-10; xmax=10; %% spatial interval
L=xmax-xmin;

Nx=100; %% number of spatial discretization intervals
h=L/Nx;
x=xmin:h:xmax;

lambda=0.51;

dT=lambda*h^2
Nt=floor(T/dT);

u=zeros(1,Nx+1); %% initialization 
ur=zeros(1,Nx+1); ul=zeros(1,Nx+1);

%u(abs(x)<1)=1;   %% Initial condition N1

u(abs(x-1)<=1)=1;  %% initial condition N2
u(abs(x+1)<1)=.5; %% initial condition N2

figure(1); plot(x,u); axis([-10 10 -1 1]); drawnow;
pause(1)

for i=1:Nt
    ul(2:Nx+1)=u(1:Nx); ur(1:Nx)=u(2:Nx+1);
    u=(1-2*lambda)*u+lambda*(ul+ur);
    figure(1); plot(x,u); axis([-10 10 -1 1]); drawnow;
end;


%%% Possible data
% T=2; Nx=100; lambda=.30; init cond N1
% T=2; Nx=100; lambda=.49; init cond N1
% T=2; Nx=100; lambda=.50; init cond N1
% T=2; Nx=100; lambda=.51; init cond N1
% T=2; Nx=100; lambda=.30; init cond N2
% T=2; Nx=100; lambda=.50; init cond N2
% T=2; Nx=100; lambda=.51; init cond N2