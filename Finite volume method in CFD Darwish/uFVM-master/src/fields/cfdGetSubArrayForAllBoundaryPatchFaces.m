function phi_b = cfdGetSubArrayForAllBoundaryPatchFaces(theFieldName, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a subarray at a given cfdBoundary
%--------------------------------------------------------------------------

global Region;

if strcmp(Region.fluid.(theFieldName).type, 'surfaceScalarField')
    theNumberOfInteriorFaces = Region.mesh.numberOfInteriorFaces;
    theNumberOfFaces = Region.mesh.numberOfFaces;
    iBFaces = theNumberOfInteriorFaces+1:theNumberOfFaces;
    
    phi_b = Region.fluid.(theFieldName).phi(iBFaces);
elseif strcmp(Region.fluid.(theFieldName).type, 'volScalarField')
    theNumberOfElements = Region.mesh.numberOfElements;
    theNumberOfBFaces = Region.mesh.numberOfBFaces;
    iBElements = theNumberOfElements+1:theNumberOfElements+theNumberOfBFaces;
    
    phi_b = Region.fluid.(theFieldName).phi(iBElements);
elseif strcmp(Region.fluid.(theFieldName).type, 'volVectorField')
    if nargin==1
        theNumberOfElements = Region.mesh.numberOfElements;
        theNumberOfBFaces = Region.mesh.numberOfBFaces;
        iBElements = theNumberOfElements+1:theNumberOfElements+theNumberOfBFaces;
        
        phi_b = Region.fluid.(theFieldName).phi(iBElements, :);
    else
        theNumberOfElements = Region.mesh.numberOfElements;
        theNumberOfBFaces = Region.mesh.numberOfBFaces;
        iBElements = theNumberOfElements+1:theNumberOfElements+theNumberOfBFaces;
        
        phi_b = Region.fluid.(theFieldName).phi(iBElements, iComponent);
    end
end



