function cfdCorrectVelocityField
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function corrects the equations
%--------------------------------------------------------------------------

% Correct VelocityField @ Interior
correctVelocityInterior;

% Correct Boundary Patches
correctVelocityBoundaryPatches;

end

%------------------------------------
% Correct VelocityField @ Interior
%------------------------------------
function correctVelocityInterior

theMesh = cfdGetMesh;

iElements = 1:theMesh.numberOfElements;

DU1 = cfdGetSubArrayForInterior('DU1');
DU2 = cfdGetSubArrayForInterior('DU2');
DU3 = cfdGetSubArrayForInterior('DU3');

%  
% PP Field
%
ppGrad = cfdGetGradientSubArrayForInterior('pp');
%
DUPPGRAD = [DU1.*ppGrad(iElements,1),DU2.*ppGrad(iElements,2),DU3.*ppGrad(iElements,3)];
%
theVelocityField = cfdGetMeshField('U');
U = theVelocityField.phi;
%
U(iElements,:) = U(iElements,:) - DUPPGRAD(iElements,:);
%
theVelocityField.phi = U;
cfdSetMeshField(theVelocityField);

end

function correctVelocityBoundaryPatches

% Get mesh
theMesh = cfdGetMesh;

theNumberOfPatches = theMesh.numberOfPatches;
for iBPatch=1:theNumberOfPatches
   
    theBCInfo = theMesh.cfdBoundaries(iBPatch);
    thePhysicalType = theBCInfo.type;
    %
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'slip')
            cfdCorrectMdotAndVelocityWallSlipBC(iBPatch);    
        elseif strcmp(theBCType,'noSlip') || strcmp(theBCType,'zeroGradient')
            cfdCorrectMdotAndVelocityWallNoslipBC(iBPatch);              
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % INLET
    %
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'inlet') || strcmp(theBCType,'zeroGradient')
            cfdCorrectMdotAndVelocityInletInletBC(iBPatch);    
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % OUTLET
    %
    elseif strcmp(thePhysicalType,'outlet') 
        if strcmp(theBCType,'outlet')
            cfdCorrectMdotAndVelocityOutletOutletBC(iBPatch); 
        elseif(strcmp(theBCType,'fixedValue')) 
            cfdCorrectMdotAndVelocityOutletFixedValueBC(iBPatch); 
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % SYMMETRY
    %
    elseif strcmp(thePhysicalType,'symmetry')
        if strcmp(theBCType,'symmetry')
          cfdCorrectMdotAndVelocitySymmetryBC(iBPatch);  
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % ERROR
    %
    elseif strcmp(thePhysicalType,'empty')
           cfdCorrectMdotAndVelocityEmptyBC(iBPatch);  
       
    else
        error([thePhysicalType ' physical condition not implemented']);
    end        
    
end


end





%===================================================
% WALL-slip
%===================================================
function cfdCorrectMdotAndVelocityWallSlipBC(iBPatch)

theMesh = cfdGetMesh;
theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;
%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

iBElementStart = numberOfElements+iFaceStart-numberOfInteriorFaces;
iBElementEnd = iBElementStart+numberOfBFaces-1;
iBElements = iBElementStart:iBElementEnd;

iOwners = [theMesh.faces(iBFaces).iOwner];

theVelocityField = cfdGetMeshField('U');
U = theVelocityField.phi;

% Update velocity
Sb = [theMesh.faces(iBFaces).Sf]';
normSb = cfdMagnitude(Sb);
n = [Sb(:,1)./normSb, Sb(:,2)./normSb, Sb(:,3)./normSb];

U_normal_cfdMag = dot(U(iOwners,:)',n')';
U_normal = [U_normal_cfdMag .* n(:,1),U_normal_cfdMag .* n(:,2),U_normal_cfdMag .* n(:,3)];

U(iBElements,:) = U(iOwners,:) - U_normal;

theVelocityField.phi = U;
cfdSetMeshField(theVelocityField);

end


%===================================================
% WALL-noslip
%===================================================
function cfdCorrectMdotAndVelocityWallNoslipBC(iBPatch)
%
end

%===================================================
% INLET-INLET
%===================================================
function cfdCorrectMdotAndVelocityInletInletBC(iBPatch)

end


%===================================================
% OUTLET-Outlet
%===================================================
function cfdCorrectMdotAndVelocityOutletOutletBC(iBPatch)

end

%===================================================
% OUTLET-specifiedValue
%===================================================
function cfdCorrectMdotAndVelocityOutletFixedValueBC(iBPatch)
%
theMesh = cfdGetMesh;
%
theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfBFaces = theBoundary.numberOfBFaces;
%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

%
iOwners = [theMesh.faces(iBFaces).iOwner]';    
%
% Get Velocity Field at Boundary
%
theVelocityField = cfdGetMeshField('U');
U = theVelocityField.phi;
U_b = U(iOwners,:);

theVelocityField.phi(iBFaces,:) = U_b;
cfdSetMeshField(theVelocityField);

end

function cfdCorrectMdotAndVelocitySymmetryBC(iBPatch)
%
theMesh = cfdGetMesh;
theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;
%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

iBElementStart = numberOfElements+iFaceStart-numberOfInteriorFaces;
iBElementEnd = iBElementStart+numberOfBFaces-1;
iBElements = iBElementStart:iBElementEnd;

iOwners = [theMesh.faces(iBFaces).iOwner];

theVelocityField = cfdGetMeshField('U');
U = theVelocityField.phi;

% Update velocity
Sb = [theMesh.faces(iBFaces).Sf]';
normSb = cfdMagnitude(Sb);
n = [Sb(:,1)./normSb, Sb(:,2)./normSb, Sb(:,3)./normSb];

U_normal_cfdMag = dot(U(iOwners,:)',n')';
U_normal = [U_normal_cfdMag .* n(:,1),U_normal_cfdMag .* n(:,2),U_normal_cfdMag .* n(:,3)];

U(iBElements,:) = U(iOwners,:) - U_normal;

theVelocityField.phi = U;
cfdSetMeshField(theVelocityField);

end

function cfdCorrectMdotAndVelocityEmptyBC(iBPatch)

theMesh = cfdGetMesh;
theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;
%
iFaceStart = theBoundary.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

iBElementStart = numberOfElements+iFaceStart-numberOfInteriorFaces;
iBElementEnd = iBElementStart+numberOfBFaces-1;
iBElements = iBElementStart:iBElementEnd;

iOwners = [theMesh.faces(iBFaces).iOwner];

theVelocityField = cfdGetMeshField('U');
U = theVelocityField.phi;

% Update velocity
Sb = [theMesh.faces(iBFaces).Sf]';
normSb = cfdMagnitude(Sb);
n = [Sb(:,1)./normSb, Sb(:,2)./normSb, Sb(:,3)./normSb];

U_normal_cfdMag = dot(U(iOwners,:)',n')';
U_normal = [U_normal_cfdMag .* n(:,1),U_normal_cfdMag .* n(:,2),U_normal_cfdMag .* n(:,3)];

U(iBElements,:) = U(iOwners,:) - U_normal;

theVelocityField.phi = U;
cfdSetMeshField(theVelocityField);
end
