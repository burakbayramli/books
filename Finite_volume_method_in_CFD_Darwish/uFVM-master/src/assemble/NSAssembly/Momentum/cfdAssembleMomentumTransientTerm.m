function cfdAssembleMomentumTransientTerm(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles transient term
%--------------------------------------------------------------------------

% Get time scheme
foamDict = cfdGetFoamDict;
theScheme = foamDict.fvSchemes.ddtSchemes.default;

if strcmp(theScheme,'steadyState')
    return;
elseif strcmp(theScheme,'Euler')
    assembleFirstOrderEulerTransientTerm(iComponent);
else
    error([theScheme, ' ddtScheme is incorrect']);
end

end

function assembleFirstOrderEulerTransientTerm(iComponent)

% Get info
volumes = cfdGetVolumesForElements;

% Get phi and phi_old
U = cfdGetSubArrayForInterior('U', iComponent);
U_old = cfdGetPrevTimeStepSubArrayForInterior('U', iComponent);

% Get rho and rho_old
rho = cfdGetSubArrayForInterior('rho');
rho_old = cfdGetPrevTimeStepSubArrayForInterior('rho');

deltaT = cfdGetDeltaT;

% Local fluxes
local_FluxC     =  volumes.*rho/deltaT;
local_FluxC_old = -volumes.*rho_old/deltaT;
local_FluxV     =  zeros(size(local_FluxC));

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxC     = local_FluxC;
theFluxes.FluxC_old = local_FluxC_old;
theFluxes.FluxV     = local_FluxV;
theFluxes.FluxT     = local_FluxC.*U + local_FluxC_old.*U_old;

cfdSetFluxes(theFluxes);

end

