function cfdZeroElementFLUXCoefficients
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
theNumberOfElements = cfdGetNumberOfElements;

% Face fluxes
Region.fluxes.FluxC(1:theNumberOfElements,1) = 0;
Region.fluxes.FluxV(1:theNumberOfElements,1) = 0;
Region.fluxes.FluxT(1:theNumberOfElements,1) = 0;

Region.fluxes.FluxC_old(1:theNumberOfElements,1)  = 0;
