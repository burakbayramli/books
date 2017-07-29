%
% compute the nondimensional reference state profile and make 3D grids for use in other codes
%
% --> applies to constant bouyancy frequency <-- (constant d\theta/dz)
%
% care is required here since the reference-state hydrostatic equation is different than 
% for the perturbation field.

% set the grid
grid_setup;

% set scaling parameters
scaling;

% vertical profile of potential temperature on staggered grid
Threfnot = Thnot/Thstar; % nondimensional surface potential temperature
Thref = Threfnot + z/Ro; % nondimensional
Threfu = Threfnot + zu/Ro; % nondimensional UNSTAGGERED grid
Threfd = Thref*Thstar; % dimensional

% vertical profile of pressure on staggered grid
pconst = Pnot^(R/Cp);
thbz = (Thnot/g)*N*N;
a = -g*pconst./(Cp*thbz);
Prefd = (pconst + a.*log(Thref*Thstar/Thnot)).^(Cp/R); % dimensional
Pref = Prefd/Pstar; % nondimensional

% vertical profile of temperature on staggered grid
Poo = Pnot/Pstar; % nondimensional reference surface pressure
Tref = Thref.*(Pref/Poo).^(R/Cp); % nondimensional
Trefd = Tref*Thstar; % dimensional

% vertical profile of density on staggered grid
Rhorefd = Prefd./(R*Trefd); % dimensional
Rhoref = Rhorefd/rhonot; % nondimensional

