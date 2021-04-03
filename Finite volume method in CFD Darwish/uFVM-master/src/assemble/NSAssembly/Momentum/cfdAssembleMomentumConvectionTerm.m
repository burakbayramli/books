function cfdAssembleMomentumConvectionTerm(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles convection term
%--------------------------------------------------------------------------

% Assemble Over Interior Faces
cfdAssembleConvectionTermInterior(iComponent);

% Assemble  Over Boundary Patches
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    
    % Find the Physical Type
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch('U', iBPatch);

    if strcmp(thePhysicalType,'wall')
        continue;
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleConvectionTermSpecifiedValue(iBPatch,iComponent);
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleConvectionTermZeroGradient(iBPatch,iComponent);
        else
            error([theBCType '<<<< Not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleConvectionTermSpecifiedValue(iBPatch,iComponent);        
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleConvectionTermZeroGradient(iBPatch,iComponent);
        else
            error([theBCType '<<<< Not implemented']);
        end
    elseif strcmp(thePhysicalType,'empty') || strcmp(thePhysicalType,'symmetry') || strcmp(thePhysicalType,'symmetryPlane')
        continue;
    else
        error([thePhysicalType '<<<< Not implemented']);
    end
end

end


%===================================================
% Assemble Interior Faces
%===================================================
function cfdAssembleConvectionTermInterior(iComponent)

% Get mesh info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
iFaces = 1:theNumberOfInteriorFaces;

owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

% get fields
Ui_C = cfdGetSubArrayForInterior('U', iComponent);
mdot_f = cfdGetSubArrayForInterior('mdot_f');

% linear fluxes
local_FluxCf =  max(mdot_f,0); 
local_FluxFf = -max(-mdot_f,0);

% Non-linear fluxes
local_FluxVf = zeros(size(local_FluxCf));

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iFaces,1) = local_FluxCf;
theFluxes.FluxFf(iFaces,1) = local_FluxFf;
theFluxes.FluxVf(iFaces,1) = local_FluxVf;
theFluxes.FluxTf(iFaces,1) = local_FluxCf.*Ui_C(owners_f) + local_FluxFf.*Ui_C(neighbours_f) + local_FluxVf;

cfdSetFluxes(theFluxes);

end



%===================================================
% Fixed Value
%===================================================
function theFluxes = cfdAssembleConvectionTermSpecifiedValue(iBPatch,iComponent)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get fields
Ui_C = cfdGetDataArray('U', iComponent);
Ui_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch, iComponent);
mdot_b = cfdGetSubArrayForBoundaryPatch('mdot_f', iBPatch);

% linear fluxes
local_FluxCb =  max(mdot_b,0); 
local_FluxFb = -max(-mdot_b,0);

% Non-linear fluxes
local_FluxVb = zeros(size(local_FluxCb));

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*Ui_C(owners_b) + local_FluxFb.*Ui_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end

%===================================================
% Zero Gradient
%===================================================
function theFluxes =  cfdAssembleConvectionTermZeroGradient(iBPatch,iComponent)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get fields
Ui_C = cfdGetDataArray('U', iComponent);
Ui_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch, iComponent);
mdot_b = cfdGetSubArrayForBoundaryPatch('mdot_f', iBPatch);

% linear fluxes
local_FluxCb =  max(mdot_b,0); 
local_FluxFb = -max(-mdot_b,0);

% Non-linear fluxes
local_FluxVb = zeros(size(local_FluxCb));

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*Ui_C(owners_b) + local_FluxFb.*Ui_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end




