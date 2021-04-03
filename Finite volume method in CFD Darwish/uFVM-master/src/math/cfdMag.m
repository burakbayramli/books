function m = cfdMag(v)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the cfdMagnitude of a cfdVector/cfdVector list
%--------------------------------------------------------------------------

m = sqrt(dot(v',v'))';


