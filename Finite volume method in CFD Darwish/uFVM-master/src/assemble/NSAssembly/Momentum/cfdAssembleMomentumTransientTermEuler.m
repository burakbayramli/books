function cfdAssembleMomentumTransientTermEuler(theEquationName, iComponent)
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

% Initialize fluxes
theFluxes.FLUXCE(iElements, 1)    = 0;
theFluxes.FLUXCEOLD(iElements, 1) = 0;
theFluxes.FLUXTE(iElements, 1)    = 0;

% Get phi and phi_old
phi = cfdGetSubArrayForInterior(theEquationName, iComponent);
phi_old = cfdGetPrevTimeStepSubArrayForInterior(theEquationName, iComponent);

% Get the rho field
rho = cfdGetSubArrayForInterior('rho');
rho_old = cfdGetPrevTimeStepSubArrayForInterior('rho');

if strcmp(theEquationName, 'T')
    Cp = cfdGetSubArrayForInterior('Cp');    
    Cp_old = cfdGetPrevIterSubArrayForInterior('Cp');
    
    rho = rho .* Cp;
    rho_old = rho_old .* Cp_old;
end

% Calculate element fluxes
theFluxes.FLUXCE    =   vol .* rho / deltaT;
theFluxes.FLUXCEOLD = - vol .* rho_old / deltaT;
theFluxes.FLUXTE    =   theFluxes.FLUXCE .* phi + theFluxes.FLUXCEOLD .* phi_old;

cfdSetFluxes(theFluxes);
end

