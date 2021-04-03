function phi_b = cfdGetSubArrayForBoundaryPatch(theFieldName, iBPatch, iComponent)
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
    iFaceStart = Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex;
    iFaceEnd = iFaceStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
    iBFaces = iFaceStart:iFaceEnd;
    
    phi_b = Region.fluid.(theFieldName).phi(iBFaces);
elseif strcmp(Region.fluid.(theFieldName).type, 'volScalarField')
    iElementStart = Region.mesh.numberOfElements+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex-Region.mesh.numberOfInteriorFaces;
    iElementEnd = iElementStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
    iBElements = iElementStart:iElementEnd;
    
    phi_b = Region.fluid.(theFieldName).phi(iBElements);
elseif strcmp(Region.fluid.(theFieldName).type, 'volVectorField')
    if nargin==2
        iElementStart = Region.mesh.numberOfElements+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex-Region.mesh.numberOfInteriorFaces;
        iElementEnd = iElementStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
        iBElements = iElementStart:iElementEnd;
        
        phi_b = Region.fluid.(theFieldName).phi(iBElements, :);
    else
        iElementStart = Region.mesh.numberOfElements+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex-Region.mesh.numberOfInteriorFaces;
        iElementEnd = iElementStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
        iBElements = iElementStart:iElementEnd;
        
        phi_b = Region.fluid.(theFieldName).phi(iBElements, iComponent);
    end
end



