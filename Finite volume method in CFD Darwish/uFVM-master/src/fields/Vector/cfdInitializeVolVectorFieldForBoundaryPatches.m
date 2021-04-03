function theField = cfdInitializeVolVectorFieldForBoundaryPatches(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes cfdBoundary conditions
%--------------------------------------------------------------------------

theMesh = cfdGetMesh;
theField = cfdGetModel(theFieldName);
theNumberOfPatches = theMesh.numberOfBoundaries;

for iPatch=1:theNumberOfPatches
    theBoundary = theMesh.cfdBoundaries(iPatch);
    thePhysicalType = theBoundary.type;
    theBCType = theField.bcs{iPatch}.type;
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')       
        if strcmp(theBCType,'fixedValue')
            cfdUpdateWallFixedValueBC(iPatch,theFieldName);
        elseif strcmp(theBCType,'fixedGradient')
            cfdUpdateWallFixedGradientBC(iPatch,theFieldName);
        elseif strcmp(theBCType,'zeroGradient')
            cfdUpdateWallZeroGradientBC(iPatch,theFieldName);
        elseif strcmp(theBCType,'hybrid')
            cfdUpdateWallHybridBC(iPatch,theFieldName);
        elseif strcmp(theBCType,'noSlip')
            cfdUpdateWallNoslipBC(iPatch,theFieldName);
        elseif strcmp(theBCType,'slip')
            cfdUpdateWallslipBC(iPatch,theFieldName);
        else
            error([theBCType 'Wall bc not defined']);
        end
        %
        % PATCH
        %
    elseif strcmp(thePhysicalType,'patch')
        if(strcmp(theBCType,'fixedValue'))
            cfdUpdatePatchfixedValueBC(iPatch,theFieldName);
        elseif(strcmp(theBCType,'fixedGradient'))
            cfdUpdateOutletfixedValueBC(iPatch,theFieldName);
        elseif(strcmp(theBCType,'zeroFlux')||strcmp(theBCType,'zeroGradient'))
            cfdUpdateOutletfixedValueBC(iPatch,theFieldName);
        elseif(strcmp(theBCType,'outlet')||strcmp(theBCType,'Outlet'))
            cfdUpdateOutletOutletBC(iPatch,theFieldName);
        else
            error([theBCType 'Wall bc not defined']);
        end
        %
        % INLET
        %
    elseif strcmp(thePhysicalType,'inlet')
        if(strcmp(theBCType,'fixedValue'))
            cfdUpdateInletFixedValueBC(iPatch,theFieldName);
        elseif strcmp(theBCType,'zeroGradient')
            cfdUpdateInletZeroGradientBC(iPatch,theFieldName);
        else
            error('Inlet bc not defined');
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalType,'outlet')
        if(strcmp(theBCType,'fixedValue'))
            cfdUpdateOutletFixedValueBC(iPatch,theFieldName);            
        elseif strcmp(theBCType,'outlet')
            cfdUpdateOutletOutletBC(iPatch,theFieldName);            
        elseif strcmp(theBCType,'zeroGradient')
            cfdUpdateOutletZeroGradientBC(iPatch,theFieldName);
        else
            error([theBCType 'Outlet bc not defined']);
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalType,'symmetry')
        if strcmp(theBCType,'symmetry')
            cfdUpdateSymmetrySymmetryBC(iPatch,theFieldName);
        else
            error('Symmetry bc not defined');
        end
        
    elseif strcmp(thePhysicalType,'empty')
        if strcmp(theBCType,'empty')
            cfdUpdateEmptyBC(iPatch,theFieldName);
        else
            error('Empty bc not defined');
        end
    else
        error([thePhysicalType '<<<< Physical Condition bc not defined']);
        
    end
                
end

end



%===================================================
% WALL-fixedValue
%===================================================
function cfdUpdateWallFixedValueBC(iPatch,theFieldName)
%
theMesh = cfdGetMesh;
theField = cfdGetModel(theFieldName);

theBCValue = theField.bcs{iPatch}.value;
% cfdBoundary phi values
% update the cfdBoundary values

theType = theField.type;
theLocale = cfdGetLocaleForPatch(iPatch);

theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

if strcmp(theType,'Scalar')
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement) = phi_b .* ones(numberOfBFaces,1);
    cfdSetMeshField(theMeshField);
    
elseif strcmp(theType,'Vector')
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement,:) = phi_b .* ones(numberOfBFaces,3);
    cfdSetMeshField(theMeshField);
    
end

end

%===================================================
% WALL-fixedGradient
%===================================================
function cfdUpdateWallFixedGradientBC(iPatch,theFieldName)

% Get field and mesh attributes
theField = cfdGetModel(theFieldName);
theBC = theField.bcs{iPatch};

theMesh = cfdGetMesh;
theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;
%
theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;
%
theFaces = theMesh.faces;
theElements = theMesh.elements;
%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;
%
iBElementStart = numberOfElements+iFaceStart-numberOfInteriorFaces;
iBElementEnd = iBElementStart+numberOfBFaces-1;
iBElements = iBElementStart:iBElementEnd;
%
iOwners = [theMesh.faces(iBFaces).iOwner];
%
d = [theFaces(iBFaces).centroid]' - [theElements(iOwners).centroid]';
dcfdMag = cfdMagnitude(d);
e = d./dcfdMag;

% Compute grad phi
formula = theBC.value;
theLocale = ['BPatch' num2str(iPatch)];
normGrad_b = cfdComputeFormulaAtLocale(formula, theLocale);
n = [theFaces(iBFaces).Sf]' ./ [theFaces(iBFaces).area]';
grad_b = normGrad_b .* n;

% Evaluate phi at cfdBoundary faces
phi(iBElements) = dcfdMag .* dot(grad_b', e')' + phi(iOwners);

% Update meshfield
theMeshField.phi = phi;
cfdSetMeshField(theMeshField);

end

%===================================================
% WALL-zeroFlux
%===================================================
function cfdUpdateWallZeroGradientBC(iPatch,theFieldName)

% get phi
theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;
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

iOwners = [theMesh.faces(iBFaces).iOwner];
phi(iBElements) = phi(iOwners);
% PatchFACES
%
theMeshField.phi = phi;
cfdSetMeshField(theMeshField);

end
%===================================================
% WALL-Hybrid
%===================================================
function cfdUpdateWallHybridBC(iPatch,theFieldName)
%

end


%===================================================
% WALL-noSlip
%===================================================
function cfdUpdateWallNoslipBC(iPatch,theFieldName)
%
theMesh = cfdGetMesh;
theField = cfdGetModel(theFieldName);
theBCValue = theField.bcs{iPatch}.value;
% cfdBoundary phi values
% update the cfdBoundary values
theType = theField.type;
theLocale = cfdGetLocaleForPatch(iPatch);

theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

theFieldBaseName = cfdGetBaseName(theField.name);
theMeshField = cfdGetMeshField(theField.name);
    
if strcmp(theField.name,'p')
    theMeshField.phi(iBElements) = theMeshField.phi(iOwners);
    cfdSetMeshField(theMeshField);    
elseif strcmp(theFieldBaseName,'U')    
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);    
    theMeshField.phi(startBElement:endBElement,:) = phi_b .* ones(numberOfBFaces,3);
    cfdSetMeshField(theMeshField);
end

end


%===================================================
% WALL-slip
%===================================================
function cfdUpdateWallslipBC(iPatch,theFieldName)
% get phi
theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;

theField = cfdGetModel(theFieldName);
theType = theField.type;
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

iOwners = [theMesh.faces(iBFaces).iOwner];

if strcmp(theType,'Scalar')
    phi(iBElements) = phi(iOwners);
    theMeshField.phi = phi;
    cfdSetMeshField(theMeshField);
elseif strcmp(theType,'Vector')    
    Sb = [theMesh.faces(iBFaces).Sf]';
    normSb = cfdMagnitude(Sb);
    n = [Sb(:,1)./normSb, Sb(:,2)./normSb, Sb(:,3)./normSb];
    
    phi_normal = dot(phi(iOwners,:)',n')';
        
    for iComponent=1:3
        phi(iBElements,iComponent) = phi(iOwners,iComponent) - phi_normal .* n(:,iComponent);
    end
    theMeshField.phi = phi;
    cfdSetMeshField(theMeshField);
end
end

%===================================================
% INLET-fixedValue
%===================================================
function cfdUpdateInletFixedValueBC(iPatch,theFieldName)
%
theMesh = cfdGetMesh;
theField = cfdGetModel(theFieldName);
theBCValue = theField.bcs{iPatch}.value;
% cfdBoundary phi values
% update the cfdBoundary values
theType = theField.type;
theLocale = cfdGetLocaleForPatch(iPatch);

theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

if strcmp(theType,'Scalar')
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement) = phi_b .* ones(numberOfBFaces,1);
    cfdSetMeshField(theMeshField);    
elseif strcmp(theType,'Vector')    
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement,:) = phi_b .* ones(numberOfBFaces,3);
    cfdSetMeshField(theMeshField);
end


end

%===================================================
% INLET-Inlet
%===================================================
function cfdUpdateInletZeroGradientBC(iPatch,theFieldName)
theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;
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

iOwners = [theMesh.faces(iBFaces).iOwner];
phi(iBElements) = phi(iOwners);
% PatchFACES
%
theMeshField.phi = phi;
cfdSetMeshField(theMeshField);
end

%===================================================
% OUTLET-fixedValue
%===================================================
function cfdUpdateOutletFixedValueBC(iPatch,theFieldName)

%
theMesh=cfdGetMesh;
theField = cfdGetModel(theFieldName);
theBCValue = theField.bcs{iPatch}.value;
% cfdBoundary phi values
% update the cfdBoundary values

theType = theField.type;
theLocale = cfdGetLocaleForPatch(iPatch);

theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;


if strcmp(theType,'Scalar')
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement) = phi_b .* ones(numberOfBFaces,1);
    cfdSetMeshField(theMeshField);    
elseif strcmp(theType,'Vector')    
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement,:) = phi_b .* ones(numberOfBFaces,3);
    cfdSetMeshField(theMeshField);
end


end

%===================================================
% OUTLET-Outlet
%===================================================
function cfdUpdateOutletZeroGradientBC(iPatch,theFieldName)
theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;
%
theField = cfdGetModel(theFieldName);
theType = theField.type;

% cfdBoundary phi values
% update the cfdBoundary values
theMesh = cfdGetMesh;
theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;
%
iBElementStart = numberOfElements+iFaceStart-numberOfInteriorFaces;
iBElementEnd = iBElementStart+numberOfBFaces-1;
iBElements = iBElementStart:iBElementEnd;
%
%
iOwners = [theMesh.faces(iBFaces).iOwner];
%
% update meshfield
%
if strcmp(theType,'Scalar')
    phi(iBElements) = phi(iOwners);
    theMeshField.phi = phi;
    cfdSetMeshField(theMeshField);
elseif strcmp(theType,'Vector')
    for iComponent=1:3
        theMeshField.phi(iBElements,iComponent) = phi(iOwners,iComponent);
    end
    cfdSetMeshField(theMeshField);
end

end

%===================================================
% SYMMETRY-Symmetry
%===================================================
function cfdUpdateSymmetrySymmetryBC(iPatch,theFieldName)

theMesh = cfdGetMesh;
theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;
%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;
%
iBElementStart = numberOfElements+iFaceStart-numberOfInteriorFaces;
iBElementEnd = iBElementStart+numberOfBFaces-1;
iBElements = iBElementStart:iBElementEnd;

theModelEquation = cfdGetModel(theFieldName);

if(strcmp(theModelEquation.type,'Vector'))
    %
    %
    iOwners = [theMesh.faces(iBFaces).iOwner]';
    %
    % Get Velocity Field at Boundary
    %
    theVectorField = cfdGetMeshField(theFieldName);
    phi = theVectorField.phi;    
    %
    % update velocity
    %
    Sb = [theMesh.faces(iBFaces).Sf]';
    normSb = cfdMagnitude(Sb);
    n = [Sb(:,1)./normSb, Sb(:,2)./normSb, Sb(:,3)./normSb];
    
    phi_normal = dot(phi(iOwners,:)',n')';
    
    phi(iBElements,1) = phi(iOwners,1) - phi_normal .* n(:,1);
    phi(iBElements,2) = phi(iOwners,2) - phi_normal .* n(:,2);
    phi(iBElements,3) = phi(iOwners,3) - phi_normal .* n(:,3);
    
    theVectorField.phi = phi;
    cfdSetMeshField(theVectorField);    
    
else
    theScalarField = cfdGetMeshField(theFieldName);
    phi = theScalarField.phi;    
    %
    % PatchFACES
    %
    iOwners = [theMesh.faces(iBFaces).iOwner]';
    phi(iBElements) = phi(iOwners);    
    %
    % update meshfield
    %
    theScalarField.phi = phi;
    cfdSetMeshField(theScalarField);
end

end

%===================================================
% EMPTY-empty
%===================================================
function cfdUpdateEmptyBC(iPatch,theFieldName)

theMesh = cfdGetMesh;
theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;
%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;
%
iBElementStart = numberOfElements+iFaceStart-numberOfInteriorFaces;
iBElementEnd = iBElementStart+numberOfBFaces-1;
iBElements = iBElementStart:iBElementEnd;

theModelEquation = cfdGetModel(theFieldName);

if(strcmp(theModelEquation.type,'Vector'))
    %
    %
    iOwners = [theMesh.faces(iBFaces).iOwner]';
    %
    % Get Velocity Field at Boundary
    %
    theVectorField = cfdGetMeshField(theFieldName);
    phi = theVectorField.phi;    
    %
    % update velocity
    %
    Sb = [theMesh.faces(iBFaces).Sf]';
    normSb = cfdMagnitude(Sb);
    n = [Sb(:,1)./normSb, Sb(:,2)./normSb, Sb(:,3)./normSb];
    
    phi_normal = dot(phi(iOwners,:)',n')';
    
    phi(iBElements,1) = phi(iOwners,1) - phi_normal .* n(:,1);
    phi(iBElements,2) = phi(iOwners,2) - phi_normal .* n(:,2);
    phi(iBElements,3) = phi(iOwners,3) - phi_normal .* n(:,3);
    
    theVectorField.phi = phi;
    cfdSetMeshField(theVectorField);    
    
else
    theScalarField = cfdGetMeshField(theFieldName);
    phi = theScalarField.phi;    
    %
    % PatchFACES
    %
    iOwners = [theMesh.faces(iBFaces).iOwner]';
    phi(iBElements) = phi(iOwners);    
    %
    % update meshfield
    %
    theScalarField.phi = phi;
    cfdSetMeshField(theScalarField);
end

end

%===================================================
% PATCH-fixedValue
%===================================================
function cfdUpdatePatchfixedValueBC(iPatch,theFieldName)
%

theMesh = cfdGetMesh;
theField = cfdGetModel(theFieldName);
theBCValue = theField.bcs{iPatch}.value;

theType = theField.type;
theLocale = cfdGetLocaleForPatch(iPatch);

theBoundary = theMesh.cfdBoundaries(iPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

startBFace = theBoundary.startFace;
startBElement = startBFace-numberOfInteriorFaces+numberOfElements;
endBElement = startBElement+numberOfBFaces-1;

if strcmp(theType,'Scalar')
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement) = phi_b .* ones(numberOfBFaces,1);
    cfdSetMeshField(theMeshField);    
elseif strcmp(theType,'Vector')   
    phi_b = cfdComputeFormulaAtLocale(theBCValue,theLocale,theType);
    theMeshField = cfdGetMeshField(theField.name);
    theMeshField.phi(startBElement:endBElement,:) = phi_b .* ones(numberOfBFaces,3);
    cfdSetMeshField(theMeshField);
end


end
