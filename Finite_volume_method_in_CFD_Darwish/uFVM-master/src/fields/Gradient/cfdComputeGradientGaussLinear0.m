function phiGrad = cfdComputeGradientGaussLinear0(phi)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function calculates gradient based on green gauss method
%--------------------------------------------------------------------------

% Check field type (by size)
theSize = size(phi,2);
if theSize==3
    theNumberOfComponents = 3;
else
    theNumberOfComponents = 1;
end

% Get mesh
theMesh = cfdGetMesh;

% INTERIOR FACES contribution to gradient
iFaces = 1:theMesh.numberOfInteriorFaces;

owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

Sf = cfdGetFaceSfSubArrayForInterior;
faceWeights = cfdGetFaceWeightsForInterior;

% Initialize phiGrad Array
phiGrad = zeros(theMesh.numberOfElements+theMesh.numberOfBElements,3,theNumberOfComponents);

% Accumulate face fields and areas
for iComponent=1:theNumberOfComponents    
    phi_f = faceWeights.*phi(neighbours_f,iComponent) + (1-faceWeights).*phi(owners_f,iComponent);
    for iFace=iFaces
        phiGrad(owners_f(iFace),:,iComponent)     = phiGrad(owners_f(iFace),:,iComponent)     + phi_f(iFace)*Sf(iFace,:);
        phiGrad(neighbours_f(iFace),:,iComponent) = phiGrad(neighbours_f(iFace),:,iComponent) - phi_f(iFace)*Sf(iFace,:);
    end    
end

% BOUNDARY FACES contribution to gradient
iBElements = theMesh.numberOfElements+1:theMesh.numberOfElements+theMesh.numberOfBFaces;
owners_b = cfdGetOwnersSubArrayForBoundaryPatchFaces;
phi_b = phi(iBElements,:);
Sb = cfdGetFaceSfSubArrayForBoundaryPatchFaces;
for iComponent=1:theNumberOfComponents
    for k=1:theMesh.numberOfBFaces
        phiGrad(owners_b(k),:,iComponent) = phiGrad(owners_b(k),:,iComponent) + phi_b(k)*Sb(k,:);
    end
end

% Get Average Gradient by dividing with element volume
volumes = cfdGetVolumesForElements;
for iComponent=1:theNumberOfComponents
    for iElement =1:theMesh.numberOfElements
        phiGrad(iElement,:,iComponent) = phiGrad(iElement,:,iComponent)/volumes(iElement);
    end
end

% Set cfdBoundary Gradient equal to associated element
% Gradient
phiGrad(iBElements,:,:) = phiGrad(owners_b,:,:);

end