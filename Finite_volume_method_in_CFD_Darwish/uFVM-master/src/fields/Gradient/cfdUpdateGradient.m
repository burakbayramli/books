function cfdUpdateGradient(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates gradient of the field
%--------------------------------------------------------------------------

foamDict = cfdGetFoamDict;
gradSchemes = foamDict.fvSchemes.gradSchemes.default;

if (strcmp(gradSchemes,'Gauss linear'))
    cfdUpdateGradientGaussLinear0(theFieldName);
else
    error('\n%s is incorrect\n', gradSchemes);
end

% BOUNDARY Gradients
theNumberOfBPatches = cfdGetNumberOfBPatches;

for iBPatch=1:theNumberOfBPatches
    
    % BC info
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBCInfo.type;
    
    if (strcmp(thePhysicalType,'wall'))
        updateWallGradients(iBPatch, theFieldName);
    elseif (strcmp(thePhysicalType,'inlet'))
        updateInletGradients(iBPatch, theFieldName);
    elseif (strcmp(thePhysicalType,'outlet'))
        updateOutletGradients(iBPatch, theFieldName);
    elseif (strcmp(thePhysicalType,'symmetry')) || (strcmp(thePhysicalType,'empty'))
        updateSymmetryGradients(iBPatch, theFieldName);
    else
        error('Boundary condition not recognized');
    end
end

end



%=====================================================
% WALL
%=====================================================
function updateWallGradients(iBPatch,theFieldName)

% Get Info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
faceCentroids_b = cfdGetFaceCentroidsSubArrayForBoundaryPatch(iBPatch);
elementCentroid = cfdGetCentroidsForElements;

theNumberOfElements = cfdGetNumberOfElements;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;

numberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);
startBFace = cfdGetStartingFaceIndexForBoundaryPatch(iBPatch);

theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;
theType = theMeshField.type;

if (strcmp(theType,'volVectorField'))
    theNumberOfComponents = 3;
else
    theNumberOfComponents = 1;
end

startBElement = startBFace-theNumberOfInteriorFaces+theNumberOfElements;
endBElement = startBElement+numberOfBFaces-1;

% Initialize patch gradient
grad = theMeshField.phiGradient;
grad_b = zeros(numberOfBFaces,3,theNumberOfComponents);

for iComponent=1:theNumberOfComponents    
    for iBFace=1:numberOfBFaces
        iBElement = startBElement-1+iBFace;
        iOwner = owners_b(iBFace);
        
        Cf = faceCentroids_b(iBFace,:);
        C  = elementCentroid(iOwner,:);        
        dCf = Cf - C;
        e = dCf/cfdMag(dCf);
        
        grad_b(iBFace,:,iComponent) = grad(iOwner,:,iComponent) - (grad(iOwner,:,iComponent)*e')*e + ((phi(iBElement,iComponent) - phi(iOwner,iComponent))/cfdMag(dCf))*e;
    end
end

% Store
theMeshField.phiGradient(startBElement:endBElement,:,:) = grad_b;
cfdSetMeshField(theMeshField);

end


%=====================================================
% INLET
%=====================================================
function updateInletGradients(iBPatch, theFieldName)

% Get Info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
faceCentroids_b = cfdGetFaceCentroidsSubArrayForBoundaryPatch(iBPatch);
elementCentroid = cfdGetCentroidsForElements;

theNumberOfElements = cfdGetNumberOfElements;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;

numberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);
startBFace = cfdGetStartingFaceIndexForBoundaryPatch(iBPatch);

theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;
theType = theMeshField.type;

if (strcmp(theType,'volVectorField'))
    theNumberOfComponents = 3;
else
    theNumberOfComponents = 1;
end

startBElement = startBFace-theNumberOfInteriorFaces+theNumberOfElements;
endBElement = startBElement+numberOfBFaces-1;

% Initialize patch gradient
grad = theMeshField.phiGradient;
grad_b = zeros(numberOfBFaces,3,theNumberOfComponents);

for iComponent=1:theNumberOfComponents    
    for iBFace=1:numberOfBFaces
        iBElement = startBElement-1+iBFace;
        iOwner = owners_b(iBFace);
        
        Cf = faceCentroids_b(iBFace,:);
        C  = elementCentroid(iOwner,:);         
        dCf = Cf - C;
        e = dCf/cfdMag(dCf);
        
        grad_b(iBFace,:,iComponent) = grad(iOwner,:,iComponent) - (grad(iOwner,:,iComponent)*e')*e + ((phi(iBElement,iComponent) - phi(iOwner,iComponent))/cfdMag(dCf))*e;
    end
end

% Store
theMeshField.phiGradient(startBElement:endBElement,:,:) = grad_b;
cfdSetMeshField(theMeshField);

end


%=====================================================
% OUTLET
%=====================================================
function updateOutletGradients(iBPatch,theFieldName)

% Get Info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
faceCentroids_b = cfdGetFaceCentroidsSubArrayForBoundaryPatch(iBPatch);
elementCentroid = cfdGetCentroidsForElements;

theNumberOfElements = cfdGetNumberOfElements;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;

numberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);
startBFace = cfdGetStartingFaceIndexForBoundaryPatch(iBPatch);

theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;
theType = theMeshField.type;

if (strcmp(theType,'volVectorField'))
    theNumberOfComponents = 3;
else
    theNumberOfComponents = 1;
end

startBElement = startBFace-theNumberOfInteriorFaces+theNumberOfElements;
endBElement = startBElement+numberOfBFaces-1;

% Initialize patch gradient
grad = theMeshField.phiGradient;
grad_b = zeros(numberOfBFaces,3,theNumberOfComponents);

for iComponent=1:theNumberOfComponents    
    for iBFace=1:numberOfBFaces
        iBElement = startBElement-1+iBFace;
        iOwner = owners_b(iBFace);
        
        Cf = faceCentroids_b(iBFace,:);
        C  = elementCentroid(iOwner,:);         
        dCf = Cf - C;
        e = dCf/cfdMag(dCf);
        
        grad_b(iBFace,:,iComponent) = grad(iOwner,:,iComponent) - (grad(iOwner,:,iComponent)*e')*e + ((phi(iBElement,iComponent) - phi(iOwner,iComponent))/cfdMag(dCf))*e;
    end
end

% Store
theMeshField.phiGradient(startBElement:endBElement,:,:) = grad_b;
cfdSetMeshField(theMeshField);

end

%=====================================================
% SYMMETRY
%=====================================================
function updateSymmetryGradients(iPatch,theFieldName)

end
