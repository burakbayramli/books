function cfdZeroFaceFLUXCoefficients
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

% Get mesh info
theNumberOfFaces = cfdGetNumberOfFaces;

% Face fluxes
Region.fluxes.FluxCf(1:theNumberOfFaces, 1) = 0;
Region.fluxes.FluxFf(1:theNumberOfFaces, 1) = 0;
Region.fluxes.FluxVf(1:theNumberOfFaces, 1) = 0;
Region.fluxes.FluxTf(1:theNumberOfFaces, 1) = 0;
