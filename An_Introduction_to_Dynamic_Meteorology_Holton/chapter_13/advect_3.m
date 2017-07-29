% MATLAB script:  advect_3.m
% Demonstration of implicit differencing for 1-d advection equation
% applied to a passive tracer with localizd distribution.
% Change Nx to examine dependence of amplitude and phase errors on 
% number of grid intervals.  
% Boundary points have q = 0 in this version
clear all
close all
S = 100;                    % number of time steps
Nxl = input('give number of grid intervals ');
Lx = 2;                     % length of domain
Nx = Nxl+1;                 % number of grid points  
x = linspace(0,Lx,Nx);
k = 2*pi/Lx;                % wavenumber of sinusoidal disturbance
dx = Lx/Nxl;                % grid increment in x
c = 10;                     % advection speed in m/s
q = zeros(size(x));
for n = 1:Nx
    if abs(x(n)-1/4) < 1/8
        q(n) = .25*(cos(8*pi*(x(n)-1/4))+1).^2;
    end
end ; 
qinit = q;
sig= 1.0;                   % value for c*dt/dx
dt = sig*4*dx/c;
S = 1.25 /(c*dt);           %total number time steps
t=0;
%   Coefficients for tridiagonal solver
e = ones(Nx,1);
% Define the tridiagonal matrix for finite difference equation

M = spdiags([-sig*e e +sig*e], -1:1, Nx,Nx);
M0 = spdiags([+sig*e e -sig*e],-1:1, Nx,Nx);
%

% Begin time looping with forward step every SF steps
for s = 1:S
    % solve M*q(s+1) = M0*q(s) for q  by matrix inversion
    qn = M\(M0*q');
    q = qn';                % update for new time t
    figure(1)
    
    plot(x ,q,'b' )
    xlabel('x in km'), ylabel('amplitude')
    axis([0 2  -.5 1.5])
    title('implicit finite difference solution')
end