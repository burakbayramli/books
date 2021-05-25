function phi = cfdGetOwnerFieldSubArrayForInteriorFaces(theFieldName, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a subarray of the owners of interior faces
%--------------------------------------------------------------------------

global Region;

iOwners = [Region.mesh.faces(1:Region.mesh.numberOfInteriorFaces).iOwner]';

if strcmp(Region.fluid.(theFieldName).type, 'volScalarField')
    phi = Region.fluid.(theFieldName).phi(iOwners);
elseif strcmp(Region.fluid.(theFieldName).type, 'volVectorField')
    if nargin==1
        phi = Region.fluid.(theFieldName).phi(iOwners, :);
    else
        phi = Region.fluid.(theFieldName).phi(iOwners, iComponent);
    end    
end
