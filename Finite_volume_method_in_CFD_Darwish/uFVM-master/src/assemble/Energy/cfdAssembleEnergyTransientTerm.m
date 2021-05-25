function cfdAssembleEnergyTransientTerm
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
    assembleFirstOrderEulerTransientTerm;
else
    error([theScheme, ' ddtScheme is incorrect']);
end

end

function assembleFirstOrderEulerTransientTerm

% Get info
volumes = cfdGetVolumesForElements;

% Get fields
T = cfdGetSubArrayForInterior('T');
T_old = cfdGetPrevTimeStepSubArrayForInterior('T');

rho = cfdGetSubArrayForInterior('rho');
rho_old = cfdGetPrevTimeStepSubArrayForInterior('rho');

Cp = cfdGetSubArrayForInterior('Cp');    
Cp_old = cfdGetPrevIterSubArrayForInterior('Cp');

deltaT = cfdGetDeltaT;

% Local fluxes
local_FluxC     =  volumes.*rho.*Cp/deltaT;
local_FluxC_old = -volumes.*rho_old.*Cp_old/deltaT;
local_FluxV     =  zeros(size(local_FluxC));

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxC     = local_FluxC;
theFluxes.FluxC_old = local_FluxC_old;
theFluxes.FluxV     = local_FluxV;
theFluxes.FluxT     = local_FluxC.*T + local_FluxC_old.*T_old;

cfdSetFluxes(theFluxes);

end