% This MATLAB script solves the one-dimensional convection
% equation using a finite difference algorithm.  The
% discretization uses central differences in space and forward
% Euler in time.

clear all;
close all;

% Number of points
Nx = 50;
x = linspace(0,1,Nx+1)
dx = 1/Nx;

%velocity
u=1;

% Set final time
tfinal = 10.0;

% Set timestep
dt = 0.001;

% Set initial condition
Uo = 0.75*exp(-((x-0.5)/0.1).^2)';
t = 0;

U = Uo;

% Loop until t > tfinal
while (t < tfinal)
    % Forward Euler Step
    U(2:end) = U(2:end) - dt*u*centraldiff(U(2:end));
    U(1) = U(end); % enforce periodicity

    % Increment time
    t = t + dt;

    % Plot current solution
    clf
    plot(x,Uo,'b*');
    hold on;
    plot(x,U,'*','color',[0 0.5 0]);
    xlabel('x','fontsize',16); ylabel('U','fontsize',16);
    title(sprintf('t = %f\n',t));
    axis([0, 1, -0.5, 1.5]);
    grid on;
    drawnow;
end
