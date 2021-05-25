function phi_new = cfdCorrectVectorFieldAtFinerLevel(iLevel, phi_old, PHI)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

PHI_cfdRestricted = cfdInterpolateVectorFieldToCoarserLevel(iLevel-1, phi_old);

v_H = PHI - PHI_cfdRestricted;
v_h = cfdInterpolateVectorFieldToFinerLevel(iLevel, v_H);

phi_new = phi_old + v_h;