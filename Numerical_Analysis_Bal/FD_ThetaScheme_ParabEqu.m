%%% Finite Differences Theta Scheme for Parabolic Equation

T=2; %% final time
xmin=-10; xmax=10; %% spatial interval
L=xmax-xmin;

Nx=100; %% number of spatial discretization intervals
h=L/Nx;
x=xmin:h:xmax;

lambda=0.60;
theta=0.1;

dT=lambda*h^2
Nt=floor(T/dT);

u=zeros(Nx+1,1); %% initialization 

%u(abs(x)<1)=1;   %% Initial condition N1

u(abs(x-1)<=1)=1;  %% initial condition N2
u(abs(x+1)<1)=.5; %% initial condition N2

figure(1); plot(x,u); axis([-10 10 -1 1]); drawnow;
pause(1)

%%%% Construction of matrices A and B
D=zeros(Nx-1,1);
D(1)=1-2*(1-theta)*lambda;
D(2)=(1-theta)*lambda; 
A=toeplitz(D); 
D(1)=1+2*theta*lambda;
D(2)=-theta*lambda; 
B=toeplitz(D); 


for i=1:Nt
    u(2:Nx)=B\(A*u(2:Nx));
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