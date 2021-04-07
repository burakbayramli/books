% Script: convect1d.m

clear all;

% Set-up grid
xL = -4;
xR =  4;
Nx = 40; % number of control volumes
x = linspace(xL,xR,Nx+1);

% Calculate midpoint values of x in each control volume
xmid = 0.5*(x(1:Nx) + x(2:Nx+1));

% Calculate cell size in control volumes (assumed equal)
dx = x(2) - x(1);

% Set velocity
u = 1;

% Set final time
tfinal = 100;

% Set timestep
CFL = 0.5;
dt = CFL*dx/abs(u);

% Set initial condition to U0 = exp(-x^2)
% Note: technically, we should average the initial
% distribution in each cell but I chose to just set
% the value of U in each control volume to the midpoint
% value of U0.

U = exp(-xmid.^2);
t = 0;

% Loop until t > tfinal
while (t < tfinal),

  Ubc = [U(Nx), U, U(1)]; % This enforces the periodic bc

  % Calculate the flux at each interface
  F =   0.5*    u *( Ubc(2:Nx+2) + Ubc(1:Nx+1)) ...
      - 0.5*abs(u)*( Ubc(2:Nx+2) - Ubc(1:Nx+1));

  % Calculate residual in each cell
  R = F(2:Nx+1) - F(1:Nx);

  % Forward Euler step
  U = U - (dt/dx)*R;

  % Increment time
  t = t + dt;

  % Plot current solution
  stairs(x,[U, U(Nx)]);
  axis([xL, xR, -0.5, 1.5]);
  grid on;
  drawnow;

end

% overlay exact solution
U = exp(-xmid.^2);
hold on;
stairs(x,[U, U(Nx)], 'r-');
