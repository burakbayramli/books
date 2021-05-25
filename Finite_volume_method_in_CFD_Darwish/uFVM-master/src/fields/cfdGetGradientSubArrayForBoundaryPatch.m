function phiGrad_b = cfdGetGradientSubArrayForBoundaryPatch(theFieldName, iBPatch, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the gradient subarray at a given cfdBoundary
%--------------------------------------------------------------------------

global Region;

if strcmp(Region.fluid.(theFieldName).type, 'surfaceScalarField')
    iFaceStart = Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex;
    iFaceEnd = iFaceStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
    iBFaces = iFaceStart:iFaceEnd;
    
    phiGrad_b = Region.fluid.(theFieldName).phiGradient(iBFaces, :);
elseif strcmp(Region.fluid.(theFieldName).type, 'volScalarField')
    iElementStart = Region.mesh.numberOfElements+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex-Region.mesh.numberOfInteriorFaces;
    iElementEnd = iElementStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
    iBElements = iElementStart:iElementEnd;
    
    phiGrad_b = Region.fluid.(theFieldName).phiGradient(iBElements, :);
elseif strcmp(Region.fluid.(theFieldName).type, 'volVectorField')
    if nargin==2
        iElementStart = Region.mesh.numberOfElements+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex-Region.mesh.numberOfInteriorFaces;
        iElementEnd = iElementStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
        iBElements = iElementStart:iElementEnd;
        
        phiGrad_b = Region.fluid.(theFieldName).phiGradient(iBElements, :, :);
    else
        iElementStart = Region.mesh.numberOfElements+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.startFaceIndex-Region.mesh.numberOfInteriorFaces;
        iElementEnd = iElementStart+Region.mesh.cfdBoundaryPatchesArray{iBPatch}.numberOfBFaces-1;
        iBElements = iElementStart:iElementEnd;
        
        phiGrad_b = Region.fluid.(theFieldName).phiGradient(iBElements, :, iComponent);
    end
end