function phi = cfdGetSubArrayForInterior(theFieldName, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a subarray at interior elements
%--------------------------------------------------------------------------

global Region;

if strcmp(Region.fluid.(theFieldName).type, 'surfaceScalarField')
    phi = Region.fluid.(theFieldName).phi(1:Region.mesh.numberOfInteriorFaces);
elseif strcmp(Region.fluid.(theFieldName).type, 'volScalarField')
    phi = Region.fluid.(theFieldName).phi(1:Region.mesh.numberOfElements);
elseif strcmp(Region.fluid.(theFieldName).type, 'volVectorField')
    if nargin==1
        phi = Region.fluid.(theFieldName).phi(1:Region.mesh.numberOfElements, :);
    else
        phi = Region.fluid.(theFieldName).phi(1:Region.mesh.numberOfElements, iComponent);
    end    
end
