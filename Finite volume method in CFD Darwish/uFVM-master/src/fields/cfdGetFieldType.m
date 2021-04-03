function theFieldType = cfdGetFieldType(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Get field type
%--------------------------------------------------------------------------

global Region;

theFieldType = Region.fluid.(theFieldName).type;