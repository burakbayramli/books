% This MATLAB script calculates the eigenvalues of
% the one-dimensional convection equation discretized by
% finite differences.  The discretization uses central
% differences in space and forward Euler in time.
%
% Periodic bcs are set if periodic_flag == 1.
%
% Otherwise, an inflow (dirichlet) bc is set and at
% the outflow a one-sided (backwards) difference is used.
%

clear all;

periodic_flag = 1;

% Set-up grid
xL = -4;
xR =  4;
Nx = 21; % number of points
x = linspace(xL,xR,Nx);

% Calculate cell size in control volumes (assumed equal)
dx = x(2) - x(1);

% Set velocity
u = 1;

% Set timestep
CFL = 1;
dt = CFL*dx/abs(u);

% Set bc state at left (assumes u>0)
UL = exp(-xL^2);

% Allocate matrix to hold stiffness matrix (A).
%
A = zeros(Nx-1,Nx-1);

% Construct A except for first and last row
for i = 2:Nx-2,
  A(i,i-1) =  u/(2*dx);
  A(i,i+1) = -u/(2*dx);
end

if (periodic_flag == 1), % Periodic bcs

  A(1   ,2   ) = -u/(2*dx);
  A(1   ,Nx-1) =  u/(2*dx);
  A(Nx-1,1   ) = -u/(2*dx);
  A(Nx-1,Nx-2) =  u/(2*dx);

else % non-periodic bc's

  % At the first interior node, the i-1 value is known (UL).
  % So, only the i+1 location needs to be set in A.
  A(1,2) = -u/(2*dx);

  % Outflow boundary uses backward difference
  A(Nx-1,Nx-2) =  u/dx;
  A(Nx-1,Nx-1) = -u/dx;

end

% Calculate eigenvalues of A
lambda = eig(A);

% Plot lambda*dt
plot(lambda*dt,'*');
xlabel('Real \lambda\Delta t');
ylabel('Imag \lambda\Delta t');

% Overlay Forward Euler stability region
th = linspace(0,2*pi,101);
hold on;
plot(-1 + sin(th),cos(th));
hold off;
axis('equal');
grid on;
