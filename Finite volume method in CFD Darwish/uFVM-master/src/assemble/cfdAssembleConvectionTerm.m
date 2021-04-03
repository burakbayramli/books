function cfdAssembleConvectionTerm(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles convection term of scalar equation
%--------------------------------------------------------------------------

% Assemble Over Interior Faces
cfdAssembleConvectionTermInterior(theEquationName);

% Assemble  Over Boundary Patches
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    
    % Find the Physical Type
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch(theEquationName, iBPatch);

    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'slip') 
            cfdAssembleConvectionTermZeroGradient(theEquationName, iBPatch);
        elseif strcmp(theBCType,'fixedValue')
            cfdAssembleConvectionTermSpecifiedValue(theEquationName, iBPatch);            
        elseif strcmp(theBCType,'noSlip')
            continue;
        else
            error([theBCType '<<<< Not implemented']);
        end
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleConvectionTermSpecifiedValue(theEquationName, iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleConvectionTermZeroGradient(theEquationName, iBPatch);
        else
            error([theBCType '<<<< Not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleConvectionTermSpecifiedValue(theEquationName, iBPatch);        
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleConvectionTermZeroGradient(theEquationName, iBPatch);
        else
            error([theBCType '<<<< Not implemented']);
        end
    elseif strcmp(thePhysicalType,'empty') || strcmp(thePhysicalType,'symmetry')
        continue;
    else
        error([thePhysicalType '<<<< Not implemented']);
    end
end

end


%===================================================
% Assemble Interior Faces
%===================================================
function cfdAssembleConvectionTermInterior(theEquationTerm)

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
iFaces = 1:theNumberOfInteriorFaces;

owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

% get fields
phi = cfdGetSubArrayForInterior(theEquationTerm);
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
theFluxes.FluxTf(iFaces,1) = local_FluxCf.*phi(owners_f) + local_FluxFf.*phi(neighbours_f) + local_FluxVf;

cfdSetFluxes(theFluxes);

end



%===================================================
% Fixed Value
%===================================================
function theFluxes = cfdAssembleConvectionTermSpecifiedValue(theEquationName, iBPatch)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get fields
phi_C = cfdGetDataArray(theEquationName);
phi_b = cfdGetSubArrayForBoundaryPatch(theEquationName, iBPatch);
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
theFluxes.FluxTf(iBFaces) = local_FluxCb.*phi_C(owners_b) + local_FluxFb.*phi_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end

%===================================================
% Zero Gradient
%===================================================
function theFluxes =  cfdAssembleConvectionTermZeroGradient(theEquationName, iBPatch)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get fields
phi_C = cfdGetDataArray(theEquationName);
phi_b = cfdGetSubArrayForBoundaryPatch(theEquationName, iBPatch);
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
theFluxes.FluxTf(iBFaces) = local_FluxCb.*phi_C(owners_b) + local_FluxFb.*phi_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end