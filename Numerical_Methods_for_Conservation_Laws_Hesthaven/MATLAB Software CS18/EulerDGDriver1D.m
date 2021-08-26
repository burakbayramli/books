% Driver script for solving the 1D wave equations using an DG scheme
clear all

% Order of method (m), number of elements (N)
m=1; N=256;
    
% Set problem parameters
FinalTime = 1.8; CFL = 0.1; gamma = 1.4;

% Define domain, materials and initial conditions
r = zeros(m+1,N); ru = zeros(m+1,N); E = zeros(m+1,N);

% Define spatial grid
xmin = -5; xmax = 5; L = xmax - xmin;

% Generate mesh
VX = (xmax-xmin)*(0:N)/N + xmin; rv = LegendreGL(m);
x = ones(m+1,1)*VX(1:N) + 0.5*(rv+1)*(VX(2:N+1)-VX(1:N));
h = (xmax-xmin)/N;

% Initialize for Sod's problem
% r = (x<0.5) + (x>=0.5)*0.125;
% E = ((x<0.5) + (x>=0.5)*0.1)/(gamma-1);

% Initialize for shock entropy problem
r = (x<-4)*3.857143 + (x>=-4).*(1+0.2*sin(pi*x));
ru = (x<-4)*3.857143*2.629369;
p = (x<-4)*10.33333 + (x>=-4);
E = p/(gamma-1) + 0.5*ru.^2./r;

q = zeros(m+1,N,3); q(:,:,1)=r; q(:,:,2)=ru; q(:,:,3)=E;
% Solve Problem
[q] = EulerDG1D(x,q,h,m,N,CFL,gamma,FinalTime);