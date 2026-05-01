% dsmceq - Dilute gas simulation using DSMC algorithm
% This version illustrates the approach to equilibrium
clear all;  help dsmceq;   % Clear memory and print header

%* Initialize constants  (particle mass, diameter, etc.)
boltz = 1.3806e-23;    % Boltzmann's constant (J/K)
mass = 6.63e-26;       % Mass of argon atom (kg)
diam = 3.66e-10;       % Effective diameter of argon atom (m)
T = 273;               % Temperature (K)
density = 1.78;        % Density of argon at STP (kg/m^3)
L = 1e-6;              % System size is one micron
npart = input('Enter number of simulation particles: ');
eff_num = density/mass*L^3/npart;
fprintf('Each particle represents %g atoms\n',eff_num);

%* Assign random positions and velocities to particles
rand('state',0);       % Initialize random number generator
x = L*rand(npart,1);   % Assign random positions
v_init = sqrt(3*boltz*T/mass);    % Initial speed
v = zeros(npart,3);    % Only x-component is non-zero
v(:,1) = v_init * (1 - 2*floor(2*rand(npart,1)));

%* Plot the initial speed distribution
figure(1);  clf;
vmag = sqrt(v(:,1).^2 + v(:,2).^2 + v(:,3).^2);
vbin = 50:100:1050;    % Bins for histogram
hist(vmag,vbin);  title('Initial speed distribution');
xlabel('Speed (m/s)');  ylabel('Number');

%* Initialize variables used for evaluating collisions
ncell = 15;                     % Number of cells
tau = 0.2*(L/ncell)/v_init;     % Set timestep tau
vrmax = 3*v_init*ones(ncell,1); % Estimated max rel. speed
selxtra = zeros(ncell,1);       % Used by routine "colider"
coeff = 0.5*eff_num*pi*diam^2*tau/(L^3/ncell);
coltot = 0;                     % Count total collisions

%* Declare structure for lists used in sorting
sortData = struct('ncell',ncell,    ...
                  'npart',npart,    ...
                  'cell_n',zeros(ncell,1),  ...
                  'index',zeros(ncell,1),    ...
                  'Xref',zeros(npart,1));  

%* Loop for the desired number of time steps
nstep = input('Enter total number of time steps: ');
for istep = 1:nstep
	
  %* Move all the particles ballistically
  x(:) = x(:) + v(:,1)*tau; % Update x position of particle
  x = rem(x+L,L);           % Periodic boundary conditions

  %* Sort the particles into cells
  sortData = sorter(x,L,sortData);
  
  %* Evaluate collisions among the particles
  [v, vrmax, selxtra, col] = ...
                colider(v,vrmax,tau,selxtra,coeff,sortData);
  coltot = coltot + col;
  
  %* Periodically display the current progress
  if( rem(istep,10) < 1 )
    figure(2); clf;
    vmag = sqrt(v(:,1).^2 + v(:,2).^2 + v(:,3).^2);
    hist(vmag,vbin);
    title(sprintf('Done %g of %g steps; %g collisions',...
                                      istep,nstep,coltot));
    xlabel('Speed (m/s)');  ylabel('Number');
    drawnow;
  end
end

%* Plot the histogram of the final speed distribution
figure(2); clf;
vmag = sqrt(v(:,1).^2 + v(:,2).^2 + v(:,3).^2);
hist(vmag,vbin);  
title(sprintf('Final distrib., Time = %g sec.',nstep*tau));
xlabel('Speed (m/s)');  ylabel('Number');
