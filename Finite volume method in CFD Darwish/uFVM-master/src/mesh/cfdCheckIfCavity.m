function cfdCheckIfCavity
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if the case is a cavity case (no specified
%   pressure)
%--------------------------------------------------------------------------

theNumberOfBoundaryPatches = cfdGetNumberOfBPatches;

% Check if mesh is closed (cavity). Loop over the boundary patches and
% check if no inlet or outlets exist. If true, then this case is said to be
% a cavity case where a pressure is not fixed and has to be fixed later
foundPatch = false;
for iBPatch=1:theNumberOfBoundaryPatches
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    if strcmp(theBCInfo.type, 'outlet') || strcmp(theBCInfo.type, 'inlet')
        foundPatch = true;
        break;
    end    
end

% Store
mesh = cfdGetMesh;
%
if foundPatch
    mesh.closed = false;
else
    mesh.closed = true;
end
%
cfdSetMesh(mesh);