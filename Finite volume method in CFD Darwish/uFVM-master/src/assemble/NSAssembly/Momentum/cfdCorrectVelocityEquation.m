function cfdCorrectVelocityEquation(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Correct velocity equation
%--------------------------------------------------------------------------

cfdCorrectVelocityForInterior(iComponent);
cfdCorrectVelocityForBoundaryPatches(iComponent);