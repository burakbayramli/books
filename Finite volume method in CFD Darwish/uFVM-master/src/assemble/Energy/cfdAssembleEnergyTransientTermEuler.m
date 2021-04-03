function cfdAssembleEnergyTransientTermEuler
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function calculates the transient fluxes based on Euler's method
%   and assembles them to the global matrix
%--------------------------------------------------------------------------

% Get fluxes
theFluxes = cfdGetFluxes;

% Get mesh and geometry info
theMesh = cfdGetMesh;
iElements = 1:theMesh.numberOfElements;
vol = [theMesh.elements(iElements).volume]';

deltaT = cfdGetDeltaT;

% Get phi and phi_old
T = cfdGetSubArrayForInterior('T');
T_old = cfdGetPrevTimeStepSubArrayForInterior('T');

% Get the rho field
rho = cfdGetSubArrayForInterior('rho');
rho_old = cfdGetPrevTimeStepSubArrayForInterior('rho');

Cp = cfdGetSubArrayForInterior('Cp');    
Cp_old = cfdGetPrevIterSubArrayForInterior('Cp');

% Calculate element fluxes
theFluxes.FLUXCE    =   vol .* rho .* Cp / deltaT;
theFluxes.FLUXCEOLD = - vol .* rho_old .* Cp_old / deltaT;
theFluxes.FLUXTE    =   theFluxes.FLUXCE .* T + theFluxes.FLUXCEOLD .* T_old;

cfdSetFluxes(theFluxes);


