function cfdSetDPhi(dphi)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Set linear system correction dphi
%--------------------------------------------------------------------------

theCoefficients = cfdGetCoefficients;
theCoefficients.dphi = dphi;
cfdSetCoefficients(theCoefficients);