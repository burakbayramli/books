function faceCF = cfdGetFaceCFSubArrayForInterior
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

theNumberOfInteriorFaces = Region.mesh.numberOfInteriorFaces;
faceCF = Region.mesh.faceCF(1:theNumberOfInteriorFaces,:);
