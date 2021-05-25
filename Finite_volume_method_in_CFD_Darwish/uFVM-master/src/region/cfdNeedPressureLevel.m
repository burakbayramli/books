function state = cfdNeedPressureLevel
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Return if the case requires pressure level. This is true when the case
%   is a closed case such that there's no pressure value specified by user
%--------------------------------------------------------------------------

state = cfdIsClosedCavity;