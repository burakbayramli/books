function theField = cfdGetMeshField(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the mesh field
%--------------------------------------------------------------------------

global Region;

if isfield(Region.fluid, theFieldName)
    theField = Region.fluid.(theFieldName);
else
    theField = -1;
end