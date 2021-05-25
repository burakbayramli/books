function cfdAssembleMassDivergenceAdvectionTerm
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

% Assemble mdot_f at interior faces
cfdAssembleMassDivergenceAdvectionTermInterior;

% Assemble at cfdBoundary patch faces
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    
    % Find the Physical Type
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);

    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'noSlip')
             cfdAssembleMassDivergenceAdvectionTermWallNoslipBC(iBPatch);
        elseif strcmp(theBCType,'slip')
             cfdAssembleMassDivergenceAdvectionTermWallSlipBC(iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
             cfdAssembleMassDivergenceAdvectionTermWallZeroGradientBC(iBPatch);
        else
            error([theBCType '<<<< not implemented']);
        end
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'inlet') || strcmp(theBCType,'zeroGradient')
             cfdAssembleMassDivergenceAdvectionTermInletZeroGradientBC(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
             cfdAssembleMassDivergenceAdvectionTermInletFixedValueBC(iBPatch);
        else
            error([theBCType '<<<< not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'outlet') || strcmp(theBCType,'zeroGradient')
             cfdAssembleMassDivergenceAdvectionTermOutletZeroGradientBC(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
             cfdAssembleMassDivergenceAdvectionTermOutletFixedValueBC(iBPatch);
        else
            error([theBCType '<<<< not implemented']);
        end
    elseif (strcmp(thePhysicalType,'empty')) || (strcmp(thePhysicalType,'symmetry')) || (strcmp(thePhysicalType,'symmetryPlane'))
        continue;
    else
        error([thePhysicalType '<<<< not implemented']);
    end
    %
end

end

%===================================================
% INTERIOR Assembly
%===================================================
function cfdAssembleMassDivergenceAdvectionTermInterior

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
iFaces = 1:theNumberOfInteriorFaces;

% Get fields
mdot_f = cfdGetSubArrayForInterior('mdot_f');

drhodp = cfdGetSubArrayForInterior('drhodp');
drhodp_f = cfdInterpolateFromElementsToInteriorFaces('linearUpwind', drhodp, mdot_f);

rho = cfdGetSubArrayForInterior('rho');
rho_f = cfdInterpolateFromElementsToInteriorFaces('linearUpwind', rho, mdot_f);

local_FluxCf =  drhodp_f./rho_f.*max( mdot_f,0);
local_FluxFf = -drhodp_f./rho_f.*max(-mdot_f,0);
local_FluxVf = -mdot_f;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iFaces) = theFluxes.FluxCf(iFaces) + local_FluxCf;
theFluxes.FluxFf(iFaces) = theFluxes.FluxFf(iFaces) + local_FluxFf;
theFluxes.FluxVf(iFaces) = theFluxes.FluxVf(iFaces) + local_FluxVf;

cfdSetFluxes(theFluxes);

end

%===================================================
% WALL- slip Conditions
%===================================================
function  cfdAssembleMassDivergenceAdvectionTermWallSlipBC(iBPatch)

end

%===================================================
% WALL-noslip Condition
%===================================================
function  cfdAssembleMassDivergenceAdvectionTermWallNoslipBC(iBPatch)

end

%===================================================
% WALL-zeroGradient Condition
%===================================================
function  cfdAssembleMassDivergenceAdvectionTermWallZeroGradientBC(iBPatch)

end

%===================================================
% INLET - Zero Gradient
%===================================================
function  cfdAssembleMassDivergenceAdvectionTermInletZeroGradientBC(iBPatch)

% Get info
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get Fields
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
mdot_b = cfdGetSubArrayForBoundaryPatch('mdot_f', iBPatch);
drhodp_b = cfdGetSubArrayForBoundaryPatch('drhodp', iBPatch);

% Local fluxes
local_FluxCb = drhodp_b.*mdot_b./rho_b;
local_FluxVb = -mdot_b;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = theFluxes.FluxCf(iBFaces) + local_FluxCb;
theFluxes.FluxVf(iBFaces) = theFluxes.FluxVf(iBFaces) + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% OUTLET - Zero Gradient
%===================================================
function  cfdAssembleMassDivergenceAdvectionTermOutletZeroGradientBC(iBPatch)
% Get info
iBFaces = getBFaceIndicesForBoundaryPatch(iBPatch);

% Get Fields
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
mdot_b = cfdGetSubArrayForBoundaryPatch('mdot_f', iBPatch);
drhodp_b = cfdGetSubArrayForBoundaryPatch('drhodp', iBPatch);

% Local fluxes
local_FluxCb = drhodp_b.*mdot_b./rho_b;
local_FluxVb = -mdot_b;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = theFluxes.FluxCf(iBFaces) + local_FluxCb;
theFluxes.FluxVf(iBFaces) = theFluxes.FluxVf(iBFaces) + local_FluxVb;

cfdSetFluxes(theFluxes);

end



%===================================================
% OUTLET - Fixed Value
%===================================================

function  cfdAssembleMassDivergenceAdvectionTermOutletFixedValueBC(iBPatch)

% Get info
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get Fields
p_b = cfdGetSubArrayForBoundaryPatch('p', iBPatch);
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
mdot_b = cfdGetSubArrayForBoundaryPatch('mdot_f', iBPatch);
drhodp_b = cfdGetSubArrayForBoundaryPatch('drhodp', iBPatch);

% Local fluxes
local_FluxFb = drhodp_b.*mdot_b./rho_b;
local_FluxVb = -mdot_b + mdot_b./rho_b.*(rho_b-drhodp_b.*p_b);

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxFf(iBFaces) = theFluxes.FluxFf(iBFaces) + local_FluxFb;
theFluxes.FluxVf(iBFaces) = theFluxes.FluxVf(iBFaces) + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% INLET - Fixed Value
%===================================================
function  cfdAssembleMassDivergenceAdvectionTermInletFixedValueBC(iBPatch)

% Get info
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get Fields
p_b = cfdGetSubArrayForBoundaryPatch('p', iBPatch);
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
mdot_b = cfdGetSubArrayForBoundaryPatch('mdot_f', iBPatch);
drhodp_b = cfdGetSubArrayForBoundaryPatch('drhodp', iBPatch);

% Local fluxes
local_FluxFb = drhodp_b.*mdot_b./rho_b;
local_FluxVb = mdot_b./rho_b.*(rho_b-drhodp_b.*p_b);

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxFf(iBFaces) = theFluxes.FluxFf(iBFaces) + local_FluxFb;
theFluxes.FluxVf(iBFaces) = theFluxes.FluxVf(iBFaces) + local_FluxVb;

cfdSetFluxes(theFluxes);

end
