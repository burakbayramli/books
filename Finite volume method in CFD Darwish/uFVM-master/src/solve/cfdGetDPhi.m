function dphi = cfdGetDPhi
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Get linear system correction
%--------------------------------------------------------------------------

theCoefficients = cfdGetCoefficients;
dphi = theCoefficients.dphi;