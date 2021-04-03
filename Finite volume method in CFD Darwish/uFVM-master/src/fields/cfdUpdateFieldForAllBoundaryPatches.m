function cfdUpdateFieldForAllBoundaryPatches(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates fields at all cfdBoundary patches
%--------------------------------------------------------------------------

theNumberOfBPatches = cfdGetNumberOfBPatches;

for iBPatch=1:theNumberOfBPatches
    theBCInfo = theMesh.cfdBoundaries(iBPatch);
    thePhysicalPatchType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch(theFieldName, iBPatch);
    %
    % WALL
    %
    if (strcmp(thePhysicalPatchType,'wall'))
        if (strcmp(theBCType,'fixedValue'))
            updateWallFixedValueBC(iBPatch,theFieldName);
        elseif (strcmp(theBCType,'zeroGradient'))
            updateWallZeroGradientBC(iBPatch,theFieldName);
        elseif (strcmp(theBCType,'noSlip'))
            updateWallNoslipBC(iBPatch,theFieldName);
        elseif (strcmp(theBCType,'slip'))
            updateWallslipBC(iBPatch,theFieldName);
        else
            error([theBCType 'Wall bc not defined']);
        end
        %
        % INLET
        %
    elseif (strcmp(thePhysicalPatchType,'inlet'))
        if (strcmp(theBCType,'fixedValue'))
            updateInletFixedValueBC(iBPatch,theFieldName);
        elseif (strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'Inlet'))
            updateInletZeroGradientBC(iBPatch,theFieldName);
        else
            error('Inlet bc not defined');
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalPatchType,'outlet')
        if strcmp(theBCType,'fixedValue')
            updateOutletFixedValueBC(iBPatch,theFieldName);            
        elseif strcmp(theBCType,'outlet') || strcmp(theBCType,'zeroGradient')
            updateOutletZeroGradientBC(iBPatch,theFieldName);            
        else
            error([theBCType 'Outlet bc not defined']);
        end
        %
        % SYMMETRY
        %
    elseif (strcmp(thePhysicalPatchType,'symmetry'))
        updateSymmetrySymmetryBC(iBPatch,theFieldName);
        %
        % EMPTY
        %        
    elseif (strcmp(thePhysicalPatchType,'empty')|| strcmp(theBCType,'Empty'))
        updateEmptyBC(iBPatch,theFieldName);
    else
        error([thePhysicalPatchType '<<<< Physical Condition bc not defined']);
        
    end
                
end

end



%===================================================
% WALL-fixedValue
%===================================================
function updateWallFixedValueBC(iBPatch,theFieldName)

% Get mesh
theMesh = cfdGetMesh;
theMeshField = cfdGetMeshField(theFieldName);

theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

% Boundary condition
theBCValue = theMeshField.cfdBoundaryPatchRef{iBPatch}.value;

% Update
theMeshField = cfdGetMeshField(theMeshField.name);
for iBElement=startBElement:endBElement
    theMeshField.phi(iBElement,:) = theBCValue;
end
cfdSetMeshField(theMeshField); 

end

%===================================================
% WALL-zeroFlux
%===================================================
function updateWallZeroGradientBC(iBPatch,theFieldName)

end
%===================================================
% WALL-Hybrid
%===================================================
function updateWallHybridBC(iBPatch,theFieldName)
%

end


%===================================================
% WALL-noSlip
%===================================================
function updateWallNoslipBC(iBPatch,theFieldName)

% Get mesh
theMesh = cfdGetMesh;
theMeshField = cfdGetMeshField(theFieldName);

theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

% Boundary condition
theBCValue = theMeshField.cfdBoundaryPatchRef{iBPatch}.value;

% Update
theMeshField = cfdGetMeshField(theMeshField.name);
for iBElement=startBElement:endBElement
    theMeshField.phi(iBElement,:) = theBCValue;
end
cfdSetMeshField(theMeshField); 

end


%===================================================
% WALL-slip
%===================================================
function updateWallslipBC(iBPatch,theFieldName)

end

%===================================================
% INLET-fixedValue
%===================================================
function updateInletFixedValueBC(iBPatch,theFieldName)

% Get mesh
theMesh = cfdGetMesh;
theMeshField = cfdGetMeshField(theFieldName);

theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

% Boundary condition
theBCValue = theMeshField.cfdBoundaryPatchRef{iBPatch}.value;

% Update
theMeshField = cfdGetMeshField(theMeshField.name);
for iBElement=startBElement:endBElement
    theMeshField.phi(iBElement,:) = theBCValue;
end
cfdSetMeshField(theMeshField); 

end

%===================================================
% INLET-Inlet
%===================================================
function updateInletZeroGradientBC(iBPatch,theFieldName)

end
%===================================================
% OUTLET-fixedValue
%===================================================
function updateOutletFixedValueBC(iBPatch,theFieldName)

% Get mesh
theMesh = cfdGetMesh;
theMeshField = cfdGetMeshField(theFieldName);

theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

% Boundary condition
theBCValue = theMeshField.cfdBoundaryPatchRef{iBPatch}.value;

% Update
theMeshField = cfdGetMeshField(theMeshField.name);
for iBElement=startBElement:endBElement
    theMeshField.phi(iBElement,:) = theBCValue;
end
cfdSetMeshField(theMeshField); 

end
%===================================================
% OUTLET-Outlet
%===================================================
function updateOutletZeroGradientBC(iBPatch,theFieldName)

end

%===================================================
% SYMMETRY-Symmetry
%===================================================
function updateSymmetrySymmetryBC(iBPatch,theFieldName)

end

%===================================================
% EMPTY-empty
%===================================================
function updateEmptyBC(iBPatch,theFieldName)

end
