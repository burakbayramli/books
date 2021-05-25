function cfdCorrectPPField
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function corrects the pressure correction field
%--------------------------------------------------------------------------

% Correct at interior
correctPPInterior;

% Correct at cfdBoundary patches
correctPPBoundaryPatches;

% Update gradient of pressure correction field
cfdUpdateGradient('pp');
end

%===================================================
% Correct Interior
%===================================================
function correctPPInterior

thePPField = cfdGetMeshField('pp');

iFixedPressureIndex = getFixedElement;

pp  = thePPField.phi;
dphi = cfdGetDPhi;

% Loop over all cells and update the field: phi@n+1 = phi@n + dphi@n+1
theMesh = cfdGetMesh;
theNumberOfElements = theMesh.numberOfElements;
iElements = 1:theNumberOfElements;

pp(iElements) = dphi;
dphi = zeros(theNumberOfElements,1);

ppc = 0;
if(iFixedPressureIndex>0)
    ppc = pp(iFixedPressureIndex);
end
cfdSetDPhi(dphi);
%
thePPField.phi(iElements) = pp(iElements) - ppc;
cfdSetMeshField(thePPField);
end

%===================================================
% Correct Boundary Patches
%===================================================
function correctPPBoundaryPatches

% Get mesh
theMesh = cfdGetMesh;

theNumberOfPatches = theMesh.numberOfPatches;
for iBPatch=1:theNumberOfPatches
    
    theBCInfo = theMesh.cfdBoundaries(iBPatch);
    thePhysicalType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'slip') || strcmp(theBCType,'noSlip') || strcmp(theBCType,'zeroGradient')
            correctPPWallBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % INLET
        %
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'inlet') || strcmp(theBCType,'zeroGradient')
            correctPPInletInletBC(iBPatch);
        elseif(strcmp(theBCType,'fixedValue'))
            correctPPInletFixedValueBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'outlet')
            correctPPOutletOutletBC(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
            correctPPOutletFixedValueBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalType,'symmetry')
        if strcmp(theBCType,'symmetry')
            correctPPSymmetryBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % EMPTY
        %
    elseif strcmp(thePhysicalType,'empty')
        if strcmp(theBCType,'empty')
            correctPPEmptyBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
    else
        error('Physical Boundary condition Not Implemented');
        
    end
    %
end
%
end


%===================================================
% WALL-noslip Wall
%===================================================
function correctPPWallBC(iPatch)
%
thePPField = cfdGetMeshField('pp');
phi = thePPField.phi;

%
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
phi(iBElements) = phi(iOwners);

%
thePPField.phi = phi;
cfdSetMeshField(thePPField);
end

%===================================================
% INLET-INLET
%===================================================
function correctPPInletInletBC(iPatch)
%
thePPField = cfdGetMeshField('pp');
phi = thePPField.phi;

%
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
phi(iBElements) = phi(iOwners);

%
thePPField.phi = phi;
cfdSetMeshField(thePPField);
end


%===================================================
% INLET-specifiedValue
%===================================================
function correctPPInletFixedValueBC(iPatch)
%
thePPField = cfdGetMeshField('pp');
phi = thePPField.phi;

%
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iPatch);

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
phi(iBElements) = 0;

%
thePPField.phi = phi;
cfdSetMeshField(thePPField);
end


%===================================================
% OUTLET-Outlet
%===================================================
function correctPPOutletOutletBC(iPatch)
%

thePPField = cfdGetMeshField('pp');
phi = thePPField.phi;

%
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
phi(iBElements) = phi(iOwners);

%
thePPField.phi = phi;
cfdSetMeshField(thePPField);
end

%===================================================
% OUTLET-specifiedValue
%===================================================
function correctPPOutletFixedValueBC(iPatch)
%

thePPField = cfdGetMeshField('pp');
phi = thePPField.phi;

%
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iPatch);

startBFace = theBoundary.startFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
phi(iBElements) = 0;

%
thePPField.phi = phi;
cfdSetMeshField(thePPField);
end


%===================================================
% SYMMETRY
%===================================================
function correctPPSymmetryBC(iPatch)
%

thePPField = cfdGetMeshField('pp');
phi = thePPField.phi;

%
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
phi(iBElements) = phi(iOwners);

%
thePPField.phi = phi;
cfdSetMeshField(thePPField);
end

%===================================================
% Empty
%===================================================
function correctPPEmptyBC(iPatch)
%

thePPField = cfdGetMeshField('pp');
phi = thePPField.phi;

%
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
phi(iBElements) = phi(iOwners);

%
thePPField.phi = phi;
cfdSetMeshField(thePPField);
end