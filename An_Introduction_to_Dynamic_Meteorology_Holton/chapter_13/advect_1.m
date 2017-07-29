% MATLAB script:  advect_1.m
% Demonstration of leapfrog differencing for 1-d linear advection equation
% applied to a passive tracer with sinusoidal distribution.
% Change Nx to examine dependence of amplitude and phase errors on 
% number of grid intervals per wavelength.
clear all
close all
S = 2*64;                   % number of time steps
SF = 16;                    % interval between forward steps
Nxl = input('give number of grid intervals per wavelength as power of 2 ')
Lx = 3.e6;                  % length of domain
Nx = Nxl+1;                 % number of grid points  
x = linspace(0,Lx,Nx);
k = 2*pi/Lx;                % wavenumber of sinusoidal disturbance
dx = Lx/Nxl;                % grid increment in x
c = 10;                     % advection speed in m/s
q = sin(k*x);               % initial tracer distribution
q0 = q;                     % q0 is tracer for time t-dt
qn = q ;                    % qn is tracer for time t+dt
sig = 0.5;                  % value for U*dt/dx
dt = sig*dx/c;
t = 0;
%Begin time looping with forward step every SF steps
for s = 1:S
    tt = s/SF;
    if rem(tt,1) == 0;
        qn(2:Nxl) = q(2:Nxl)-sig/2*(q(3:Nx)-q(1:Nx-2)); % interior points
        qn(1) = q(1)-sig/2*(q(2)-q(Nxl));               % boundary points
        qn(Nx) = qn(1);
    else
        qn(2:Nxl) = q0(2:Nxl)-sig*(q(3:Nx)-q(1:Nx-2));  % interior points
        qn(1) = q0(1)-sig*(q(2)-q(Nxl));                % boundary points
        qn(Nx) = qn(1);
    end
    t = t+dt;
    q0 = q;                                     % update for new time t - dt
    q = qn;                                     % update for new time t
    qexact = sin(k*(x-c*t));
    figure(1)
    plot(x/1000,q,'b' ,x/1000,q-qexact,'g', x/1000, qexact,'r')
    xlabel('x in km'), ylabel('amplitude')
    axis([0 3000 -1.5 1.5])
    title('blue finite difference, red exact, green error')
end