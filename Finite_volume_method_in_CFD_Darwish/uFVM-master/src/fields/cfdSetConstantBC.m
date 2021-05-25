function cfdSetConstantBC(theFieldName, theBoundaryCondition)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function sets up a constant cfdBoundary condition for a field
%--------------------------------------------------------------------------

theMeshField = cfdGetMeshField(theFieldName);

theNumberOfBPatches = cfdGetNumberOfBPatches;

for iBPatch=1:theNumberOfBPatches
    theMeshField.boundaryPatchRef{iBPatch}.type = theBoundaryCondition;        
end

cfdSetMeshField(theMeshField);