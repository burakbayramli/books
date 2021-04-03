function theScfdUrfaces = getScfdUrfacesSubArrayForBoundaryPatch(iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

iFaceStart = Region.mesh.cfdBoundaries(iBPatch).startFace;
iFaceEnd = iFaceStart+Region.mesh.cfdBoundaries(iBPatch).numberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

theScfdUrfaces = [Region.mesh.faces(iBFaces).Sf]';
