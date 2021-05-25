function cfdSetCurrentTime(currentTime)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates current time
%--------------------------------------------------------------------------
global Region;

Region.time.currentTime = currentTime;