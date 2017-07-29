%
% A spectral quasigeostrophic model (see README for details)
% 
% Originator: G.J. Hakim, University of Washington
%
% released under GNU General Public License version 3. http://www.gnu.org/licenses/gpl.html
%
% version control:
% $Date: 2012-02-10 12:09:59 -0800 (Fri, 10 Feb 2012) $
% $Revision: 790 $
% $Author: hakim $
% $Id: QG_model.m 790 2012-02-10 20:09:59Z hakim $

clear

% global variables
global Nx Ny Nz DX DY facx facy dz z

% switches to save output and use a simple jet (1=yes, 0=no)
isave     = 0; % save data to a unique .mat file for plotting & diagnostics
ijet      = 1; % include a basic-state jet in the simulation

% create the numerical grid
grid_setup;

% the model is non-dimensional, and the output is scaled as follows
scaling;

% set the model time step
dt = 15*minute; % first number is minutes; dt is seconds

% set the time interval for integration and the interval for saving data
tspan = [0:6*hr:96*hr]; % numbers are hours; tspan is seconds

% nondimensional time step and time interval
dt = dt/Tstar;       % nondimensional
tspan = tspan/Tstar; % nondimensinal

% option to include a linear shear jet (U = lamda z + Unot) 
if ijet
  lambda = 1; Unot = 3.75/U;
end

% Specify the initial-time disturbance field (arbitrary PV & boundary theta)
pvmag = -2e-6; % PV units
thmag = 0; % K
ipv = 5; % select a preset disturbance
QG_initial_value;

% Everything in/out of QG_tend should be a column vector
PVvec = reshape(pvxy,Nx*Ny*Nz,1); 
TBvec = reshape(lbcxy,Nx*Ny,1);
TTvec = reshape(ubcxy,Nx*Ny,1);
Xi = [PVvec; TBvec; TTvec];

%
% run the model (solution at times in T are columns of Xf)
%

disp([' ']); disp(['running the QG model...'])

tic

if ijet
  % with a basic-state jet
  [T,Xf] = advance_rk4(@QG_tend,dt,tspan,Xi,'lambda',lambda,'Unot',Unot);
else
  % without a basic-state jet
  [T,Xf] = advance_rk4(@QG_tend,dt,tspan,Xi);
end

toc 

% option to save the solution data to a unique filename
if isave
  filen = strcat('QGout_',datestr(now,'yyyymmdd.HHMMSS'),'.mat');
  % save data for plotting and record scripts that generated essential data
  if isunix
	 [s,grid_setup_script] = unix('cat grid_setup.m');
	 [s,scaling_script] = unix('cat scaling.m');
	 [s,reference_state_script] = unix('cat reference_state.m');
	 [s,QG_initial_value_script] = unix('cat QG_initial_value.m');
  else
	 [s,grid_setup_script] = dos('type grid_setup.m');
	 [s,scaling_script] = dos('type scaling.m');
	 [s,reference_state_script] = dos('type reference_state.m');
	 [s,QG_initial_value_script] = dos('type QG_initial_value.m');
  end
  save(filen,'T','Xf','tspan','grid_setup_script','scaling_script','reference_state_script','QG_initial_value_script','ijet','lambda','Unot');
end


