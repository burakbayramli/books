clear all; close all; clc;

% specify domain
a = 0;
b = 2*pi;

% initial condition: u(x,0) = alpha + beta*sin(x)
alpha = 0.0;
beta  = 1.0;

% number of grid points in spatial discretization
N  = 320;

% stopping time
T = 1.5;
    
% setup grid points
x = linspace(a,b,N)';     
dx = (b-a)/(N-1);   

u = zeros(length(x)-1,1); 
    
% compute cell averages at t=0
for i=1:N-1
    u(i) = 1/dx*integral(@(xx)alpha+beta*sin(xx),x(i),x(i+1),'ArrayValued',true);
end  
    
% time step
dt = dx/(2*max(max(u)));

t = 0.0;
while (t<T)
    A  = max(max(u));

    % compute numerical fluxes fhat_{j+1/2}
    um = u; up = circshift(u,-1);
    fR = numerical_flux(um,up,dx,dt,A);    
    
    % compute numerical fluxes fhat_{j-1/2} (assuming periodic BCs)
    fL = circshift(fR,1);

    % first order explicit time-stepping
    u = u - dt/dx*(fR - fL);

    t = t+dt;
end

% compute cell averages of exact solution
Ne = 300;
xe = linspace(a,b,Ne)'; 
ue = zeros(length(xe)-1,1);

dxe = (b-a)/(Ne-1); 

for i=1:Ne-1
    ue(i) = 1/dxe*integral(@(xx)exact_solution(t,xx',alpha,beta),xe(i),xe(i+1),'ArrayValued',true);
end  

% plot results
figure
plot(x,[u; u(1)])
hold on;
plot(xe,[ue; ue(1)])
