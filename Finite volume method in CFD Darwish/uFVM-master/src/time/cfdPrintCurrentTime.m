function cfdPrintCurrentTime
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function prints current simulation time
%--------------------------------------------------------------------------

global Region;

fprintf('\n\n%s %d \n', 'Time: ', Region.time.currentTime);