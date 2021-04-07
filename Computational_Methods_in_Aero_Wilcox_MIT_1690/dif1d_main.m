% MATLAB script: dif1d_main.m
%
% This code solve the one-dimensional heat diffusion equation
% for the problem of a bar which is initially at T=Tinit and
% suddenly the temperatures at the left and right change to
% Tleft and Tright.
%
% Upon discretization in space by a finite difference method,
% the result is a system of ODE's of the form,
%
% u_t = Au + b
%
% The code calculates A and b.  Then, uses one of MATLAB's
% ODE integrators, either ode45 (which is based on a Runge-Kutta
% method and is not designed for stiff problems) or ode15s (which
% is based on an implicit method and is designed for stiff problems).
%

clear all; close all;

sflag = input('Use stiff integrator? (1=yes, [default=no]): ');

% Set non-dimensional thermal coefficient
k   = 1.0; % this is really k/(rho*cp)

% Set length of bar
L = 1.0; % non-dimensional

% Set initial temperature
Tinit = 400;

% Set left and right temperatures for t>0
Tleft = 800;
Tright = 1000;

% Set up grid size
Nx = input(['Enter number of divisions in x-direction: [default=' ...
            '51]']);
if (isempty(Nx)),
  Nx = 51;
end

h  = L/Nx;
x  = linspace(0,L,Nx+1);

% Calculate number of iterations (Nmax) needed to iterate to t=Tmax
Tmax = 0.5;

% Initialize a sparse matrix to hold stiffness & identity matrix
A = spalloc(Nx-1,Nx-1,3*(Nx-1));
I = speye(Nx-1);

% Calculate stiffness matrix

for ii = 1:Nx-1,

  if (ii > 1),
    A(ii,ii-1) = k/h^2;
  end

  if (ii < Nx-1),
    A(ii,ii+1) = k/h^2;
  end

  A(ii,ii) = -2*k/h^2;

end

% Set forcing vector
b = zeros(Nx-1,1);
b(1)    = k*Tleft/h^2;
b(Nx-1) = k*Tright/h^2;

% Set initial vector
v0 = Tinit*ones(1,Nx-1);

if (sflag == 1),

  % Call ODE15s
  options = odeset('Jacobian',A);
  [t,v] = ode15s(@dif1d_fun,[0 Tmax],v0,options,A,b);

else

  % Call ODE45
  [t,v] = ode45(@dif1d_fun,[0 Tmax],v0,[],A,b);

end

% Get midpoint value of T and plot vs. time
Tmid = v(:,floor(Nx/2));
plot(t,Tmid);
xlabel('t');
ylabel('T at midpoint');
