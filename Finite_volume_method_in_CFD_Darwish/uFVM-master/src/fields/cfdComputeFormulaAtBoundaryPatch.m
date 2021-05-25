function phi_b = cfdComputeFormulaAtBoundaryPatch(iBPatch, theValue, valueType)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function evaluates field at cfdBoundary patch
%--------------------------------------------------------------------------

theMesh = cfdGetMesh;

theBoundary = theMesh.cfdBoundaries(iBPatch);
numberOfElements = theMesh.numberOfElements;
numberOfInteriorFaces = theMesh.numberOfInteriorFaces;
numberOfBFaces = theBoundary.numberOfBFaces;

phi_b = 

