clear all; close all; clc;

% specify domain
a = 0;
b = 2*pi;

% initial condition: u(x,0) = alpha + beta*sin(x)
alpha = 0.0;
beta  = 1.0;

% number of grid points in spatial discretization
N  = [20 40 80 160 320 640]+1;

% stopping time
T = 1.5;

% TVB limiter parameter
M = 1.0;
    
for k=1:length(N)
    % setup grid points
    x = linspace(a,b,N(k))';     
    dx = (b-a)/(N(k)-1);   

    u0 = zeros(length(x)-1,1); 

    % compute cell averages at t=0
    for i=1:N(k)-1
        u0(i) = 1/dx*integral(@(xx)alpha+beta*sin(xx),x(i),x(i+1),...
                              'ArrayValued',true);
    end  

    % time step
    dt = dx/(2*max(max(u0)));

    iter = 0;
    t = 0.0;
    while (t<T)
        A  = max(max(u0));

        % first RK stage
        [um,up] = polynomial_reconstruction(u0);
        [um,up] = tvb_limiter(um,up,u0,dx,M);
        fR = numerical_flux(um,up,dx,dt,A);    
        fL = circshift(fR,1);
        u = u0 - dt/dx*(fR - fL);

        % second RK stage
        [um,up] = polynomial_reconstruction(u);
        [um,up] = tvb_limiter(um,up,u,dx,M);
        fR = numerical_flux(um,up,dx,dt,A);    
        fL = circshift(fR,1);
        u = 3/4*u0 + 1/4*(u - dt/dx*(fR - fL));

        % third RK stage
        [um,up] = polynomial_reconstruction(u);
        [um,up] = tvb_limiter(um,up,u,dx,M);
        fR = numerical_flux(um,up,dx,dt,A);    
        fL = circshift(fR,1);
        u = 1/3*u0 + 2/3*(u - dt/dx*(fR - fL));

        u0 = u;
        t = t+dt;
        
        iter = iter+1;
    end
    
    
    ue = zeros(length(x)-1,1);

    for i=1:N(k)-1
        ue(i) = 1/dx*integral(@(xx)exact_solution(t,xx',alpha,beta),x(i),x(i+1),...
                               'ArrayValued',true);
    end 

    % compute numerical errors outside of the shock
    d = pi+alpha*t - 2*pi*floor((pi+alpha*t)/(2*pi));
    ii = find((x>d+0.5) | (x<d-0.5));
    L1(k) = sum(abs(u(ii(1:end-1))-ue(ii(1:end-1))))/length(ii(1:end-1));  
    
end

for k=1:length(N)-1
    rateL1(k) = log2(L1(k)/L1(k+1));
end


