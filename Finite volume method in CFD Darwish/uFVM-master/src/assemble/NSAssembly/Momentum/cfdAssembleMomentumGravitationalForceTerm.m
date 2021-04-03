function cfdAssembleMomentumGravitationalForceTerm(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles buoyancy term
%--------------------------------------------------------------------------

% Get info
volumes = cfdGetVolumesForElements;

% Get fields
rho = cfdGetSubArrayForInterior('rho');

% Get gravity
gi = cfdGetGravity(iComponent);

% Update and store
theFluxes = cfdGetFluxes;
theFluxes.FluxT = rho.*gi.*volumes;
cfdSetFluxes(theFluxes);
