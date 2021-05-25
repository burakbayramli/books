function cfdUpdateVectorFieldForAllBoundaryPatches(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates scalar field at all cfdBoundary patches
%--------------------------------------------------------------------------

theNumberOfBPatches = cfdGetNumberOfBPatches;

for iBPatch=1:theNumberOfBPatches
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalPatchType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch(theFieldName, iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalPatchType,'wall')
        if strcmp(theBCType,'fixedValue')
            updateFixedValue(iBPatch,theFieldName);
        elseif strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'noSlip') || strcmp(theBCType,'slip')
            updateZeroGradient(iBPatch,theFieldName);
        else
            error([theBCType ' bc not defined']);
        end
        %
        % INLET
        %
    elseif (strcmp(thePhysicalPatchType,'inlet'))
        if (strcmp(theBCType,'fixedValue'))
            updateFixedValue(iBPatch,theFieldName);
        elseif strcmp(theBCType,'zeroGradient')
            updateZeroGradient(iBPatch,theFieldName);
        else
            error('Inlet bc not defined');
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalPatchType,'outlet')
        if strcmp(theBCType,'fixedValue')
            updateFixedValue(iBPatch,theFieldName);            
        elseif strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'outlet')
            updateZeroGradient(iBPatch,theFieldName);            
        else
            error([theBCType 'Outlet bc not defined']);
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalPatchType,'symmetry')
        updateSymmetry(iBPatch,theFieldName);
        %
        % EMPTY
        %        
    elseif strcmp(thePhysicalPatchType,'empty')
        updateSymmetry(iBPatch,theFieldName);
    else
        error([thePhysicalPatchType '<<<< Physical Condition bc not defined']);
        
    end
                
end

end



%===================================================
% Fixed Value
%===================================================
function updateFixedValue(iBPatch, theFieldName)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);

% Get field
theVectorField = cfdGetMeshField(theFieldName);

% Apply cfdBoundary condition
theBCValue = cfdValueForBoundaryPatch(theFieldName, iBPatch);
if size(theBCValue,1)>1
    theVectorField.phi(iBElements,:) = theBCValue;
else
    theVectorField.phi(iBElements,:) = repmat(theBCValue,length(iBElements),1);
end

% Store
cfdSetMeshField(theVectorField);

end


%===================================================
% Zero Gradient
%===================================================
function updateZeroGradient(iBPatch, theFieldName)

% Get info
theVectorField = cfdGetMeshField(theFieldName);

iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

% Apply cfdBoundary condition
theVectorField.phi(iBElements,:) = theVectorField.phi(owners_b,:);

% Store
cfdSetMeshField(theVectorField);

end


%===================================================
% Symmetry
%===================================================
function updateSymmetry(iBPatch, theFieldName)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

% Get field
theVectorField = cfdGetMeshField(theFieldName);

% Update bounadry condition
Sb = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
normSb = cfdMag(Sb);
n = [Sb(:,1)./normSb, Sb(:,2)./normSb, Sb(:,3)./normSb];

U_normal_cfdMag = dot(theVectorField.phi(owners_b,:)',n')';
U_normal = [U_normal_cfdMag .* n(:,1),U_normal_cfdMag .* n(:,2),U_normal_cfdMag .* n(:,3)];

theVectorField.phi(iBElements,:) = theVectorField.phi(owners_b,:) - U_normal;

% Store
cfdSetMeshField(theVectorField);

end
