function cfdStartTime
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function starts time
%--------------------------------------------------------------------------

global Region;
Region.cfdStartTime = double(tic);
Region.currentTime = Region.cfdStartTime;