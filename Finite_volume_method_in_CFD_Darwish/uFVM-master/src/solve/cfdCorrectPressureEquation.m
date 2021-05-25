function cfdCorrectPressureEquation
%===================================================

%  written by the CFD Group @ AUB, Fall 2006
%===================================================

% Correct Pressure @ interior
correctPressureInterior;

% Correct Pressure @ BC
correctPressureBoundaryPatches;

end


%===================================================
% Correct Interior
%===================================================
function correctPressureInterior

% Get mesh info
theMesh = cfdGetMesh;
theNumberOfElements = theMesh.numberOfElements;
iElements = 1:theNumberOfElements;

% Get pressure field
thePressureField = cfdGetMeshField('p');
p = thePressureField.phi;

% Get pressure correction field
thePPField  = cfdGetMeshField('pp');
pp = thePPField.phi;

% Get pressure under-relaxation
foamDict = cfdGetFoamDict;
URFP = foamDict.fvSolution.relaxationFactors.('p');
%
%
ppFixed = 0;
iFixedElement = getFixedElement;
if(iFixedElement>0)
    ppFixed = pp(iFixedElement);
end
pp = pp - ppFixed;

% Update pressure field with explicit under-relaxation
p(iElements) =  p(iElements) + URFP*pp(iElements);

thePressureField.phi = p;
cfdSetMeshField(thePressureField);
end


%===================================================
% Correct Boundary Patches
%===================================================
function correctPressureBoundaryPatches

theMesh = cfdGetMesh;
theNumberOfPatches = theMesh.numberOfBoundaries;

for iBPatch=1:theNumberOfPatches
    
    theBoundary = theMesh.cfdBoundaries(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'slip') || strcmp(theBCType,'noSlip') || strcmp(theBCType,'zeroGradient')
            correctPressureWallBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % INLET
        %
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'inlet') || strcmp(theBCType,'zeroGradient')
            correctPressureInletInletBC(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
            correctPressureInletFixedValueBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'outlet')
            correctPressureOutletOutletBC(iBPatch);
        elseif(strcmp(theBCType,'fixedValue'))
            correctPressureOutletFixedValueBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalType,'symmetry')
        if strcmp(theBCType,'symmetry')
            correctPressureSymmetryBC(iBPatch);
        else
            error([theBCType 'BC Condition not Implemented']);
        end
        %
        % EMPTY
        %
    elseif strcmp(thePhysicalType,'empty')
        correctPressureEmptyBC(iBPatch);
    else
        %
        % ERROR
        %
        error('Physical Condition in Pressure Correction not Implemented');
    end
    
end


end

%===================================================
% WALL
%===================================================
function correctPressureWallBC(iBPatch)
%
thePressureField = cfdGetMeshField('p');
p = thePressureField.phi;

% PatchFACES
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iBPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
p(iBElements) = p(iOwners);
%
thePressureField.phi = p;
cfdSetMeshField(thePressureField);
end

%===================================================
% INLET-INLET
%===================================================
function correctPressureInletInletBC(iBPatch)
%
thePressureField = cfdGetMeshField('p');
p = thePressureField.phi;

% PatchFACES
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iBPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
p(iBElements) = p(iOwners);
%
thePressureField.phi = p;
cfdSetMeshField(thePressureField);
end


%===================================================
% INLET-specifiedValue
%===================================================
function correctPressureInletFixedValueBC(iBPatch)
%


end


%===================================================
% OUTLET-Outlet
%===================================================
function correctPressureOutletOutletBC(iBPatch)
%
%
thePressureField = cfdGetMeshField('p');
p = thePressureField.phi;

% PatchFACES
theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iBPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
p(iBElements) = p(iOwners);
%
thePressureField.phi = p;
cfdSetMeshField(thePressureField);
end

%===================================================
% OUTLET-specifiedValue
%===================================================
function correctPressureOutletFixedValueBC(iBPatch)


end
%===================================================
% Symmetry BC
%===================================================

function correctPressureSymmetryBC(iBPatch)

thePressureField = cfdGetMeshField('p');
p = thePressureField.phi;

theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iBPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
p(iBElements) = p(iOwners);
%
thePressureField.phi = p;
cfdSetMeshField(thePressureField);
end

%===================================================
% Empty BC
%===================================================

function correctPressureEmptyBC(iBPatch)

thePressureField = cfdGetMeshField('p');
p = thePressureField.phi;

theMesh = cfdGetMesh;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;

theBoundary = theMesh.cfdBoundaries(iBPatch);

startBFace = theBoundary.startFace;
endBFace = startBFace+theBoundary.numberOfBFaces-1;
iBFaces = startBFace:endBFace;

startBElement = theMesh.numberOfElements+startBFace-numberOfInteriorFaces;
endBElement = startBElement+theBoundary.numberOfBFaces-1;
iBElements = startBElement:endBElement;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
p(iBElements) = p(iOwners);
%
thePressureField.phi = p;
cfdSetMeshField(thePressureField);
end
