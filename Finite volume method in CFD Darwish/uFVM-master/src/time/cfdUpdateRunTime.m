function cfdUpdateRunTime
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

Region.time.currentTime = Region.time.currentTime + Region.foamDictionary.controlDict.deltaT;
Region.time.cpuTime = toc;