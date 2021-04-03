function cfdAssembleEnergyDiffusionTerm
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles Diffusion term for energy equation
%--------------------------------------------------------------------------

% Assemble over Interior Faces
cfdAssembleDiffusionTermInterior;

% Assemble vver Boundary Patches
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches

    % Find the Physical Type
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch('T', iBPatch);

    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'zeroGradient')
            cfdAssembleDiffusionTermZeroGradient(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
            cfdAssembleDiffusionTermSpecifiedValue(iBPatch);
        else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleDiffusionTermSpecifiedValue(iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleDiffusionTermZeroGradient(iBPatch);
        else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleDiffusionTermSpecifiedValue(iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            cfdAssembleDiffusionTermZeroGradient(iBPatch);
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
function cfdAssembleDiffusionTermInterior

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
T = cfdGetSubArrayForInterior('T');

gradT = cfdGetGradientSubArrayForInterior('T');
gradT_f = cfdInterpolateGradientsFromElementsToInteriorFaces('linear',gradT);

% Get viscosity
k = cfdGetSubArrayForInterior('k');
k_f = cfdInterpolateFromElementsToInteriorFaces('linear',k);

% Minimum correction approach
Ef = [cfdMagSf.*e(:,1),cfdMagSf.*e(:,2),cfdMagSf.*e(:,3)];

% Calculate non-orthogonal complement of Sf
Tf = Sf - Ef;

% Geometric diffusion
geoDiff_f = cfdMag(Ef)./cfdMagCF;

% Linear fluxes
local_FluxCf =   k_f.*geoDiff_f;
local_FluxFf = - k_f.*geoDiff_f;

% Non-linear fluxes
local_FluxVf = - k_f.*dot(gradT_f(:,:)',Tf(:,:)')';

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iFaces,1) = local_FluxCf;
theFluxes.FluxFf(iFaces,1) = local_FluxFf;
theFluxes.FluxVf(iFaces,1) = local_FluxVf;
theFluxes.FluxTf(iFaces,1) = theFluxes.FluxCf(iFaces).*T(owners_f) + theFluxes.FluxFf(iFaces).*T(neighbours_f) + theFluxes.FluxVf(iFaces);

cfdSetFluxes(theFluxes);

end

%===================================================
% Specified Value
%===================================================
function cfdAssembleDiffusionTermSpecifiedValue(iBPatch)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get required fields
k_b = cfdGetSubArrayForBoundaryPatch('k', iBPatch);

T = cfdGetDataArray('T');
T_C = T(owners_b,:);
T_b = cfdGetSubArrayForBoundaryPatch('T', iBPatch);

% Geometric diffusion
geoDiff_b = cfdMag(Sf_b)./wallDist_b;

% Linear fluxes
local_FluxCb =  k_b.*geoDiff_b;
local_FluxFb = -k_b.*geoDiff_b;

% non-linear fluxes
local_FluxVb = zeros(size(local_FluxFb));

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*T_C + local_FluxFb.*T_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% Zero Gradient
%===================================================
function cfdAssembleDiffusionTermZeroGradient(iBPatch)

end