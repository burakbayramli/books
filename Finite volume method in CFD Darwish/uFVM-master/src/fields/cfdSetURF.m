function cfdSetURF(theFieldName,cfdUrf)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%   Set under-relaxation factor for field
%--------------------------------------------------------------------------

global Region;

Region.fluid.(theFieldName).cfdUrf = cfdUrf;