% dsmcne - Program to simulate a dilute gas using DSMC algorithm
% This version simulates planar Couette flow
clear all;  help dsmcne;   % Clear memory and print header

%* Initialize constants  (particle mass, diameter, etc.)
boltz = 1.3806e-23;    % Boltzmann's constant (J/K)
mass = 6.63e-26;       % Mass of argon atom (kg)
diam = 3.66e-10;       % Effective diameter of argon atom (m)
T = 273;               % Initial temperature (K)
density = 2.685e25;    % Number density of argon at STP (m^-3)
L = 1e-6;              % System size is one micron
Volume = L^3;          % Volume of the system (m^3)
npart = input('Enter number of simulation particles: ');
eff_num = density*Volume/npart;
fprintf('Each simulation particle represents %g atoms\n',eff_num);
mfp = Volume/(sqrt(2)*pi*diam^2*npart*eff_num);
fprintf('System width is %g mean free paths \n',L/mfp);
mpv = sqrt(2*boltz*T/mass);  % Most probable initial velocity 
vwall_m = input('Enter wall velocity as Mach number: ');
vwall = vwall_m * sqrt(5/3 * boltz*T/mass);
fprintf('Wall velocities are %g and %g m/s \n',-vwall,vwall);

%* Assign random positions and velocities to particles
rand('state',1);        % Initialize random number generators
randn('state',1);
x = L*rand(npart,1);    % Assign random positions
% Assign thermal velocities using Gaussian random numbers
v = sqrt(boltz*T/mass) * randn(npart,3);
% Add velocity gradient to the y-component
v(:,2) = v(:,2) + 2*vwall*(x(:)/L) - vwall;

%* Initialize variables used for evaluating collisions
ncell = 20;                   % Number of cells
tau = 0.2*(L/ncell)/mpv;      % Set timestep tau
vrmax = 3*mpv*ones(ncell,1);  % Estimated max rel. speed in a cell
selxtra = zeros(ncell,1);     % Used by collision routine "colider"
coeff = 0.5*eff_num*pi*diam^2*tau/(Volume/ncell);

%* Declare structure for lists used in sorting
sortData = struct('ncell', ncell,    ...
                  'npart', npart,    ...
                  'cell_n', zeros(ncell,1),  ...
                  'index', zeros(ncell,1),    ...
                  'Xref', zeros(npart,1));  

%* Initialize structure and variables used in statistical sampling
sampData = struct('ncell', ncell,    ...
                  'nsamp', 0,    ...
                  'ave_n', zeros(ncell,1), ...
                  'ave_u', zeros(ncell,3), ...
                  'ave_T', zeros(ncell,1));
tsamp = 0;                    % Total sampling time
dvtot = zeros(1,2);           % Total momentum change at a wall
dverr = zeros(1,2);           % Used to find error in dvtot

%* Loop for the desired number of time steps
colSum = 0;  strikeSum = [0 0];
nstep = input('Enter total number of timesteps: ');
for istep = 1:nstep
	
  %* Move all the particles
  [x, v, strikes, delv] = mover(x,v,npart,L,mpv,vwall,tau);
  strikeSum = strikeSum + strikes;

  %* Sort the particles into cells
  sortData = sorter(x,L,sortData);

  %* Evaluate collisions among the particles
  [v, vrmax, selxtra, col] = ...
          colider(v,vrmax,tau,selxtra,coeff,sortData);
  colSum = colSum + col;

  %* After initial transient, accumulate statistical samples
  if(istep > nstep/10) 
    sampData = sampler(x,v,npart,L,sampData);
    dvtot = dvtot + delv;
    dverr = dverr + delv.^2;
    tsamp = tsamp + tau;
  end

  %* Periodically display the current progress
  if( rem(istep,10) < 1 )
    fprintf('Finished %g of %g steps, Collisions = %g\n', ...
                                            istep,nstep,colSum);
    fprintf('Total wall strikes: %g (left)  %g (right)\n', ...
                                  strikeSum(1),strikeSum(2));
  end
end

%* Normalize the accumulated statistics
nsamp = sampData.nsamp;
ave_n = (eff_num/(Volume/ncell))*sampData.ave_n/nsamp;    
ave_u = sampData.ave_u/nsamp;
ave_T = mass/(3*boltz) * (sampData.ave_T/nsamp);
dverr = dverr/(nsamp-1) - (dvtot/nsamp).^2;
dverr = sqrt(dverr*nsamp);

%* Compute viscosity from drag force on the walls
force = (eff_num*mass*dvtot)/(tsamp*L^2);
ferr = (eff_num*mass*dverr)/(tsamp *L^2);
fprintf('Force per unit area is \n');
fprintf('Left wall:   %g +/- %g \n',force(1),ferr(1));  
fprintf('Right wall:  %g +/- %g \n',force(2),ferr(2));
vgrad = 2*vwall/L;  % Velocity gradient
visc = 1/2*(-force(1)+force(2))/vgrad;  % Average viscosity
viscerr = 1/2*(ferr(1)+ferr(2))/vgrad;  % Error
fprintf('Viscosity = %g +/- %g N s/m^2\n',visc,viscerr);
eta = 5*pi/32*mass*density*(2/sqrt(pi)*mpv)*mfp;
fprintf('Theoretical value of viscoisty is %g N s/m^2\n',eta);

%* Plot average density, velocity and temperature
figure(1); clf;
xcell = ((1:ncell)-0.5)/ncell * L;
plot(xcell,ave_n); xlabel('position');  ylabel('Number density');
figure(2); clf;
plot(xcell,ave_u); xlabel('position');  ylabel('Velocities');
legend('x-component','y-component','z-component');
figure(3); clf;
plot(xcell,ave_T); xlabel('position');  ylabel('Temperature');

