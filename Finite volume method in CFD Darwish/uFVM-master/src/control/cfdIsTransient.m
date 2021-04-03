function transient = cfdIsTransient
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if the case is transient
%--------------------------------------------------------------------------

global Region;

transient = ~Region.STEADY_STATE_RUN;