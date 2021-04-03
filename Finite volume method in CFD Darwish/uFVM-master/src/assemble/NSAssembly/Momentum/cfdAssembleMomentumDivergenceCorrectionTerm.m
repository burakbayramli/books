function cfdAssembleMomentumDivergenceCorrectionTerm(iComponent)
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

% Compute effective divergence
effDiv = cfdComputeEffectiveDivergence('U');

% Get fields
Ui = cfdGetSubArrayForInterior('U', iComponent);

% Get fluxes
theFluxes = cfdGetFluxes;

for iElement=1:theNumberOfElements
    theFluxes.FluxC(iElement) =   max(effDiv(iElement),0.0) - effDiv(iElement);
    theFluxes.FluxV(iElement) = - max(effDiv(iElement),0.0)*Ui(iElement);
    theFluxes.FluxT(iElement) =   theFluxes.FluxC(iElement)*Ui(iElement) + theFluxes.FluxV(iElement);     
end    

cfdSetFluxes(theFluxes);