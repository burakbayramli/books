function cfdAssemblePressureGradientTerm(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles the pressure gradient term
%--------------------------------------------------------------------------

% Get info and fields
volume = cfdGetVolumesForElements;
p_grad = cfdGetGradientSubArrayForInterior('p');

% Update global fluxes
theFluxes = cfdGetFluxes;
theFluxes.FluxT = volume.*p_grad(:,iComponent);
cfdSetFluxes(theFluxes);

end