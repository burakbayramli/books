% MATLAB script:  nonlinear_advect_diffuse.m
% Numerical solution for 1-d nonlinear advection-diffusion equation.
% Initial wind has sinusoidal distribution.
% Change Nxl to examine dependence of amplitude and phase errors on 
% number of grid intervals per wavelength.
clear all
close all
Nxl = 40;
Lx = 3.e6;                    % length of domain
Nx = Nxl+1;                  % number of grid points  
x = linspace(0,Lx,Nx);
k = 2*pi/Lx;                 % wavenumber of sinusoidal disturbance
dx = Lx/Nxl;                 % grid increment in x  (m)
u = 5*sin(k*x);              % initial velocity distribution (m/s)
dt = 1200;                   % time step in seconds
Kd = 2e5;                    % diffusion coefficient
S = fix(48*3600)/dt;         % time steps for 2 day simulation
dtdx = dt/dx;
r = 2*Kd*dt/dx^2;

t = 0;
%Begin time looping with forward step  
un(2:Nxl) = u(2:Nxl)-dtdx/2*(u(2:Nxl).*u(3:Nx)-u(1:Nx-2))...
    +r/2*(u(3:Nx)-2*u(2:Nxl)+u(1:Nx-2));        % interior points
un(1) = u(1)-dtdx/2*u(1)*(u(2)-u(Nxl))...
    +r/2*(u(2)-2*u(1)+u(Nxl));                  % boundary points
un(Nx) = un(1);
u0 = u;
u = un;
for s = 2:S
    un(2:Nxl) = u0(2:Nxl)-dtdx.*u(2:Nxl).*(u(3:Nx)-u(1:Nx-2))...
        +r*(u0(3:Nx)-2*u0(2:Nxl)+u0(1:Nx-2));   % interior points
    un(1) = u0(1)-dtdx*u(1)*(u(2)-u(Nxl))...
        +r*(u0(2)-2*u0(1)+u0(Nxl));             % boundary points
    un(Nx) = un(1);
    
    t = t+dt;
    u0 = u;                             % update for new time t - dt
    u = un;                             % update for new time t
    
    figure(1)
    
    plot(x/1000,u,'b'),
    xlabel('x in km'), ylabel('amplitude')
    axis([0 3000 -10 10])
    title('nonlinear advection diffusion equation')
end