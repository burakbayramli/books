function cfdCorrectVelocityForBoundaryPatches(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Correct velocity field at cfdBoundary patches
%--------------------------------------------------------------------------

theNumberOfBoundaries = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBoundaries
    
    % Get Physical and Equation Boundary Conditions
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch('U', iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'noSlip')
            continue;
        elseif strcmp(theBCType,'slip')
            correctVelocitySlipWall(iBPatch,iComponent);
        elseif strcmp(theBCType,'fixedValue')
            continue;            
        else
            error('Wall Condition not implemented')
        end
        %
        % INLET
        %
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            continue;
        elseif strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'inlet')
            correctVelocityZeroGradient(iBPatch,iComponent);            
        else
            error('Inlet Condition not implemented')
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'outlet') || strcmp(theBCType,'zeroGradient')
            correctVelocityZeroGradient(iBPatch,iComponent);
        elseif strcmp(theBCType,'fixedValue')
            continue;            
        else
            error([theBCType '<<<< Outlet Condition not implemented'])
        end
        %
        % SYMMETRY/EMPTY
        %
    elseif strcmp(thePhysicalType,'symmetry') || strcmp(thePhysicalType,'symmetryPlane') || strcmp(thePhysicalType,'empty')
        correctVelocitySymmetry(iBPatch,iComponent);
    else
        error('%s Condition not implemented', thePhysicalType)
    end
    
end

end

%===================================================
% Zero Gradient
%===================================================
function correctVelocityZeroGradient(iBPatch,iComponent)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
iOwners = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

% Copy value from owner cell
theVelocityField = cfdGetMeshField('U');
theVelocityField.phi(iBElements, iComponent) = theVelocityField.phi(iOwners, iComponent);

% Store
cfdSetMeshField(theVelocityField);

end

%===================================================
% Slip Wall
%===================================================
function correctVelocitySlipWall(iBPatch, iComponent)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

Sb = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
n = cfdUnit(Sb);

% Get Vector Field at Boundary
theVelocityField = cfdGetMeshField('U');
U_C = theVelocityField.phi;

% Update field by imposing the parallel to wall component of velocity. This
% is done by removing the normal component
U_normal = dot(U_C(owners_b,:)',n')';
U_C(iBElements,iComponent) = U_C(owners_b,iComponent) - U_normal.*n(:,iComponent);

% Store
theVelocityField.phi = U_C;
cfdSetMeshField(theVelocityField);

end

%===================================================
% Symmetry
%===================================================
function correctVelocitySymmetry(iBPatch,iComponent)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

Sb = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
n = cfdUnit(Sb);

% Get Vector Field at Boundary
theVelocityField = cfdGetMeshField('U');
U_C = theVelocityField.phi;

% Update field by imposing the parallel to wall component of velocity. This
% is done by removing the normal component
U_normal = dot(U_C(owners_b,:)',n')';
U_C(iBElements,iComponent) = U_C(owners_b,iComponent) - U_normal.*n(:,iComponent);

% Store
theVelocityField.phi = U_C;
cfdSetMeshField(theVelocityField);

end
