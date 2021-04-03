function theFluxes = cfdGetFluxes
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the stored fluxes
%--------------------------------------------------------------------------

global Region;

theFluxes =  Region.fluxes;


