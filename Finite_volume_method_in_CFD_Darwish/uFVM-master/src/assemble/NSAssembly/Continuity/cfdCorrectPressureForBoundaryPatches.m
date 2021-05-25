function cfdCorrectPressureForBoundaryPatches
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

theNumberOfBPatches = cfdGetNumberOfBPatches;

for iBPatch=1:theNumberOfBPatches
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalPatchType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);
    %
    % WALL
    %
    if (strcmp(thePhysicalPatchType,'wall'))
        correctPressureZeroGradient(iBPatch);
        %
        % INLET
        %
    elseif (strcmp(thePhysicalPatchType,'inlet'))
        if strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'inlet')
            correctPressureZeroGradient(iBPatch);
        else
            error('Inlet bc not defined');
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalPatchType,'outlet')
        if strcmp(theBCType,'fixedValue')
            correctPressureSpecifiedValue(iBPatch);            
        elseif strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'outlet')
            correctPressureZeroGradient(iBPatch);            
        else
            error([theBCType 'Outlet bc not defined']);
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalPatchType,'symmetry')
        correctPressureZeroGradient(iBPatch);
        %
        % EMPTY
        %        
    elseif strcmp(thePhysicalPatchType,'empty')
        correctPressureZeroGradient(iBPatch);
    else
        error([thePhysicalPatchType '<<<< Physical Condition bc not defined']);
        
    end
                
end

end



%===================================================
% Specified Value
%===================================================
function correctPressureSpecifiedValue(iBPatch)

end


%===================================================
% Zero Gradient
%===================================================
function correctPressureZeroGradient(iBPatch)

thePressureField = cfdGetMeshField('p');

iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
iOwners = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

thePressureField.phi(iBElements) = thePressureField.phi(iOwners);

cfdSetMeshField(thePressureField);

end
