function cfdZeroFluxes
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function stores the fluxes
%--------------------------------------------------------------------------

global Region;

theFluxes = Region.fluxes;

% Get mesh info
theMesh = cfdGetMesh;
theNumberOfFaces = theMesh.numberOfFaces;
theNumberOfElements = theMesh.numberOfElements;

% Face fluxes
theFluxes.FLUXC1f(1:theNumberOfFaces, 1) = 0;
theFluxes.FLUXC2f(1:theNumberOfFaces, 1) = 0;
theFluxes.FLUXVf(1:theNumberOfFaces, 1)  = 0;
theFluxes.FLUXTf(1:theNumberOfFaces, 1)  = 0;

% Volume fluxes
theFluxes.FLUXCE(1:theNumberOfElements, 1) = 0;
theFluxes.FLUXCEOLD(1:theNumberOfElements, 1) = 0;
theFluxes.FLUXTE(1:theNumberOfElements, 1) = 0;

% Store
cfdSetFluxes(theFluxes);
