function cfdAssembleDivergenceCorrectionTerm(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function claculates the divergence correction term
%--------------------------------------------------------------------------

% Get info
theNumberOfElements = cfdGetNumberOfElements;

% Get effective divergence
effDiv = cfdComputeEffectiveDivergence;

% Get fields
phi = cfdGetSubArrayForInterior(theEquationName);

% Get fluxes
theFluxes = cfdGetFluxes;

for iElement=1:theNumberOfElements
    theFluxes.FluxC(iElement,1) =   max(effDiv(iElement),0.0) - effDiv(iElement);
    theFluxes.FluxV(iElement,1) = - max(effDiv(iElement),0.0)*phi(iElement);
    theFluxes.FluxT(iElement,1) =   theFluxes.FluxC(iElement)*phi(iElement) + theFluxes.FluxV(iElement);     
end    

cfdSetFluxes(theFluxes);