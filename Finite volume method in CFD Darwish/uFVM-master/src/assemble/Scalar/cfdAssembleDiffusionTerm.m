function cfdAssembleDiffusionTerm(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles Diffusion term for scalar equation
%--------------------------------------------------------------------------

% Assemble over Interior Faces
cfdAssembleDiffusionTermInterior(theEquationName);

% Assemble vver Boundary Patches
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches

    % Find the Physical Type
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch(iBPatch);

    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'zeroGradient')
            cfdAssembleDiffusionTermZeroGradient(theEquationName, iBPatch);
        elseif strcmp(theBCType,'fixedValue')
            cfdAssembleDiffusionTermSpecifiedValue(theEquationName, iBPatch);
        else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleDiffusionTermSpecifiedValue(theEquationName, iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleDiffusionTermZeroGradient(theEquationName, iBPatch);
        else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleDiffusionTermSpecifiedValue(theEquationName, iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleDiffusionTermZeroGradient(theEquationName, iBPatch);
        else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'symmetry') || strcmp(thePhysicalType,'empty')
        continue;
    else
        error([thePhysicalType ' physical condition is not implemented']);
    end    
end

end




%===================================================
% INTERIOR
%===================================================
function cfdAssembleDiffusionTermInterior(theEquationName)

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
Sf = cfdGetFaceSfSubArrayForInterior;
CF = cfdGetFaceCFSubArrayForInterior;
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;
iFaces = 1:theNumberOfInteriorFaces;

% Calculated info
e = cfdUnit(CF);
cfdMagSf = cfdMag(Sf);
cfdMagCF = cfdMag(CF);

% Get fields
phi = cfdGetSubArrayForInterior(theEquationName);

gradPhi = cfdGetGradientSubArrayForInterior(theEquationName);
gradPhi_f = cfdInterpolateGradientsFromElementsToInteriorFaces('linear',gradPhi);

% Get gamma
gamma = cfdGetSubArrayForInterior('gamma');
gamma_f = cfdInterpolateFromElementsToInteriorFaces('linear',gamma);

% Minimum correction approach
Ef = [cfdMagSf.*e(:,1),cfdMagSf.*e(:,2),cfdMagSf.*e(:,3)];

% Calculate non-orthogonal complement of Sf
Tf = Sf - Ef;

% Geometric diffusion
geoDiff_f = cfdMag(Ef)./cfdMagCF;

% Linear fluxes
local_FluxCf =   gamma_f.*geoDiff_f;
local_FluxFf = - gamma_f.*geoDiff_f;

% Non-linear fluxes
local_FluxVf = - gamma_f.*dot(gradPhi_f(:,:)',Tf(:,:)')';

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iFaces,1) = local_FluxCf;
theFluxes.FluxFf(iFaces,1) = local_FluxFf;
theFluxes.FluxVf(iFaces,1) = local_FluxVf;
theFluxes.FluxTf(iFaces,1) = theFluxes.FluxCf(iFaces).*phi(owners_f) + theFluxes.FluxFf(iFaces).*phi(neighbours_f) + theFluxes.FluxVf(iFaces);

cfdSetFluxes(theFluxes);

end

%===================================================
% Specified Value
%===================================================
function cfdAssembleDiffusionTermSpecifiedValue(theEquationName, iBPatch)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get required fields
gamma_b = cfdGetSubArrayForBoundaryPatch('gamma', iBPatch);

phi = cfdGetDataArray(theEquationName);
phi_C = phi(owners_b,:);
phi_b = cfdGetSubArrayForBoundaryPatch(theEquationName, iBPatch);

% Geometric diffusion
geoDiff_b = cfdMag(Sf_b)./wallDist_b;

% Linear fluxes
local_FluxCb =  gamma_b.*geoDiff_b;
local_FluxFb = -gamma_b.*geoDiff_b;

% non-linear fluxes
local_FluxVb = zeros(size(local_FluxFb));

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*phi_C + local_FluxFb.*phi_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% Zero Gradient
%===================================================
function cfdAssembleDiffusionTermZeroGradient(theEquationName, iBPatch)

end