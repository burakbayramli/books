function cfdCorrectMdotField
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function corrects the mdot_f field
%--------------------------------------------------------------------------

% Correct mdot field at interior faces
cfdCorrectMdotInterior;

% Correct mdot field at cfdBoundary faces
cfdCorrectMdotBoundaryPatches;


end


%===================================================
% Correct MdotField @ INTERIOR
%===================================================
function cfdCorrectMdotInterior

theFluxes = cfdGetFluxes;
FLUXC1f = theFluxes.FLUXC1f;
FLUXC2f = theFluxes.FLUXC2f;

theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;

% Get mesh info
theMesh = cfdGetMesh;
iFaces = 1:theMesh.numberOfInteriorFaces;
iOwners = [theMesh.faces(iFaces).iOwner]';
iNeighbours = [theMesh.faces(iFaces).iNeighbour]';

% PP Field
pp = cfdGetSubArrayForInterior('pp');

% Correct
mdot_f(iFaces) = mdot_f(iFaces) + FLUXC1f(iFaces).*pp(iOwners) + FLUXC2f(iFaces).*pp(iNeighbours);

% Store mdot_f
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);

end


%===================================================
% Correct Boundary Patches
%===================================================
function cfdCorrectMdotBoundaryPatches

% get mesh info
theMesh = cfdGetMesh;
%
theNumberOfPatches = theMesh.numberOfPatches;
for iBPatch=1:theNumberOfPatches
   
    theBCInfo = theMesh.cfdBoundaries(iBPatch);
    thePhysicalType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'slip')
            cfdCorrectMdotWallSlipBC(iBPatch);    
        elseif strcmp(theBCType,'noSlip') || strcmp(theBCType,'zeroGradient')
            cfdCorrectMdotWallNoslipBC(iBPatch);              
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % INLET
    %
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'inlet') || strcmp(theBCType,'zeroGradient')
            cfdCorrectMdotInletInletBC(iBPatch);    
        elseif strcmp(theBCType,'fixedValue') 
            cfdCorrectMdotInletFixedValueBC(iBPatch);    
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % OUTLET
    %
    elseif strcmp(thePhysicalType,'outlet') 
        if strcmp(theBCType,'outlet')
            cfdCorrectMdotOutletOutletBC(iBPatch); 
        elseif(strcmp(theBCType,'fixedValue')) 
            cfdCorrectMdotOutletFixedValueBC(iBPatch); 
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % SYMMETRY
    %
    elseif strcmp(thePhysicalType,'symmetry')
        if strcmp(theBCType,'symmetry')
          cfdCorrectMdotSymmetryBC(iBPatch);  
        else
            error([theBCType ' cfdBoundary condition not implemented']);
        end
    %
    % ERROR
    %
    elseif strcmp(thePhysicalType,'empty')
           cfdCorrectMdotEmptyBC(iBPatch);         
    else
        error([thePhysicalType ' physical condition not implemented']);
    end        
    
end


end


%===================================================
% WALL-slip
%===================================================
function cfdCorrectMdotWallSlipBC(iBPatch)

% Get mesh info
theMesh = cfdGetMesh;
theBCInfo = theMesh.cfdBoundaries(iBPatch);
numberOfBFaces = theBCInfo.numberOfBFaces;
%
iFaceStart = theBCInfo.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

% update mdot
theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;
mdot_f(iBFaces) = 0.0*mdot_f(iBFaces);
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);

end


%===================================================
% WALL-noslip
%===================================================
function cfdCorrectMdotWallNoslipBC(iBPatch)
%
end

%===================================================
% INLET-INLET
%===================================================
function cfdCorrectMdotInletInletBC(iBPatch)

if ~cfdIsCompressible
    return;
end

theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;

theMesh = cfdGetMesh;
%
theBCInfo = theMesh.cfdBoundaries(iBPatch);
numberOfBFaces = theBCInfo.numberOfBFaces;

%
iFaceStart = theBCInfo.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

% Pressure correction field
pp_b = cfdGetSubArrayForBoundaryPatch('pp', iBPatch);

% Get density field
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);

% Get drhodp field
drhodp_b = cfdGetSubArrayForBoundaryPatch('drhodp', iBPatch);

% Correct by adding compressible contribution
mdot_f(iBFaces) = mdot_f(iBFaces) + (mdot_f(iBFaces) ./ rho_b) .* drhodp_b .* pp_b;

theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);


end


%===================================================
% INLET-specifiedValue
%===================================================
function cfdCorrectMdotInletFixedValueBC(iBPatch)

end


%===================================================
% OUTLET-Outlet
%===================================================
function cfdCorrectMdotOutletOutletBC(iBPatch)

theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;

% Get density field
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);

% Get mesh info
theMesh = cfdGetMesh;
%
theBCInfo = theMesh.cfdBoundaries(iBPatch);
numberOfBFaces = theBCInfo.numberOfBFaces;

%
iFaceStart = theBCInfo.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

%
% Get Velocity Field at Boundary
%
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);

% ////////////////////////////////////////////////////////////
% update mdot
Sb = [theMesh.faces(iBFaces).Sf]';
mdot_f(iBFaces) = mdot_f(iBFaces) + rho_b.*dot(U_b',Sb')';
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);

end

%===================================================
% OUTLET-specifiedValue
%===================================================
function cfdCorrectMdotOutletFixedValueBC(iBPatch)

theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;

% Get density field
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);

theMesh = cfdGetMesh;
%
theBCInfo = theMesh.cfdBoundaries(iBPatch);
numberOfBFaces = theBCInfo.numberOfBFaces;
%
iFaceStart = theBCInfo.startFace;
iFaceEnd = iFaceStart+numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

%
iOwners = [theMesh.faces(iBFaces).iOwner]';    
%
% Get Velocity Field at Boundary
%
theVelocityField = cfdGetMeshField('U');
U_b = theVelocityField.phi(iOwners,:);
%
% update velocity
%
Sb = [theMesh.faces(iBFaces).Sf]';

% update mdot
mdot_f(iBFaces) = rho_b .* dot(U_b',Sb')';
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);


end

function cfdCorrectMdotSymmetryBC(iBPatch)

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;
mdot_f(iBFaces) = 0.0*mdot_f(iBFaces);
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);

end

function cfdCorrectMdotEmptyBC(iBPatch)

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;
mdot_f(iBFaces) = 0.0*mdot_f(iBFaces);
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);

end
