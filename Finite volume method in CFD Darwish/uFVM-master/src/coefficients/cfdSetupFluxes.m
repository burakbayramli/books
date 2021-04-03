function cfdSetupFluxes
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function sets up the fluxes
%--------------------------------------------------------------------------

global Region;

% Get mesh info
theNumberOfFaces = cfdGetNumberOfFaces;
theNumberOfElements = cfdGetNumberOfElements;

% Face fluxes
Region.fluxes.FluxCf(1:theNumberOfFaces,1) = 0;
Region.fluxes.FluxFf(1:theNumberOfFaces,1) = 0;
Region.fluxes.FluxVf(1:theNumberOfFaces,1) = 0;
Region.fluxes.FluxTf(1:theNumberOfFaces,1) = 0;

% Volume fluxes
Region.fluxes.FluxC(1:theNumberOfElements,1) = 0;
Region.fluxes.FluxV(1:theNumberOfElements,1) = 0;
Region.fluxes.FluxT(1:theNumberOfElements,1) = 0;

Region.fluxes.FluxC_old(1:theNumberOfElements,1) = 0;
