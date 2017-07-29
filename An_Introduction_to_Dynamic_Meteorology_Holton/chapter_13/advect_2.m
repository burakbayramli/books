% MATLAB script:  advect_2.m
% Demonstration of leapfrog differencing for 1-d linear advection equation
% applied to a passive tracer with localized distribution.
% Change Nx to examine dependence of amplitude and phase errors on 
% number of grid intervals.  
clear all
close all
S = 100;                        % number of time steps
SF = 20;                        % interval between forward steps
Nxl = input('give number of grid intervals ');
Lx = 2;                         % length of domain
Nx = Nxl+1;                     % number of grid points  
x = linspace(0,Lx,Nx);
dx = Lx/Nxl;                    % grid increment in x
c = 10;                         % advection speed in m/s
q = zeros(size(x));
for n = 1:Nx
    if abs(x(n)-1/4) < 1/8
        q(n) = .25*(cos(8*pi*(x(n)-1/4))+1).^2;
    end
end ; 
qinit = q;
q0 = q;                         % q0 is tracer for time t-dt
qn = q;                         % qn is tracer for time t+dt
sig = .5;                       % value for c*dt/dx
dt = sig*dx/c;
S = 1.25 /(c*dt);               %total number time steps
t = 0;
%Begin time looping with forward step every SF steps
for s = 1:S
    tt = s/SF;
    if rem(tt,1) == 0;
        
        qn(2:Nxl) = q(2:Nxl)-sig/2*(q(3:Nx)-q(1:Nx-2)); %  interior points
        qn(1) = q(1)-sig/2*(q(2)-q(Nxl));   % boundary points
        qn(Nx) = qn(1);
    else
        qn(2:Nxl) = q0(2:Nxl)-sig*(q(3:Nx)-q(1:Nx-2)); %  interior points
        qn(1) = q0(1)-sig*(q(2)-q(Nxl));    % boundary points
        qn(Nx) = qn(1);
    end
    figure(1)

    t = t+dt;
    q0 = q;                 % update for new time t - dt
    q = qn;                 % update for new time t
    plot(x ,q,'b' ),
    xlabel('x in km'), ylabel('amplitude')
    axis([0 2  -.5 1.5])
    title(' 2nd order finite difference solution')
end