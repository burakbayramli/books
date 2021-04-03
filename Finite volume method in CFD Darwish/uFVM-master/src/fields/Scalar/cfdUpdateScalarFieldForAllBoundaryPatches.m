function cfdUpdateScalarFieldForAllBoundaryPatches(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function cfdUpdates scalar field at all cfdBoundary patches
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
            cfdUpdateFixedValue(iBPatch,theFieldName);
        elseif strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'noSlip') || strcmp(theBCType,'slip')
            cfdUpdateZeroGradient(iBPatch,theFieldName);
        else
            error([theBCType ' bc not defined']);
        end
        %
        % INLET
        %
    elseif strcmp(thePhysicalPatchType,'inlet')
        if strcmp(theBCType,'fixedValue')
            cfdUpdateFixedValue(iBPatch,theFieldName);
        elseif strcmp(theBCType,'zeroGradient')
            cfdUpdateZeroGradient(iBPatch,theFieldName);
        else
            error('Inlet bc not defined');
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalPatchType,'outlet')
        if strcmp(theBCType,'fixedValue')
            cfdUpdateFixedValue(iBPatch,theFieldName);            
        elseif strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'outlet')
            cfdUpdateZeroGradient(iBPatch,theFieldName);            
        else
            error([theBCType 'Outlet bc not defined']);
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalPatchType,'symmetry')
        cfdUpdateZeroGradient(iBPatch,theFieldName);
        %
        % EMPTY
        %        
    elseif strcmp(thePhysicalPatchType,'empty')
        cfdUpdateZeroGradient(iBPatch,theFieldName);
    else
        error([thePhysicalPatchType '<<<< Physical Condition bc not defined']);
        
    end
                
end

end



%===================================================
% Fixed Value
%===================================================
function cfdUpdateFixedValue(iBPatch, theFieldName)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);

% Get field
theScalarField = cfdGetMeshField(theFieldName);

% Apply cfdBoundary condition
theBCValue = cfdValueForBoundaryPatch(theFieldName, iBPatch);
theScalarField.phi(iBElements) = theBCValue;

% Store
cfdSetMeshField(theScalarField);

end


%===================================================
% Zero Gradient
%===================================================
function cfdUpdateZeroGradient(iBPatch, theFieldName)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

% Get field
theScalarField = cfdGetMeshField(theFieldName);

% Copy owner values
theScalarField.phi(iBElements) = theScalarField.phi(owners_b);

% Store
cfdSetMeshField(theScalarField);

end
