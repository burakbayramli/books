function proceed = cfdDoTransientLoop
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function cfdRuns steady state case
%--------------------------------------------------------------------------

global Region;

currentTime = Region.time.currentTime;
endTime = Region.foamDictionary.controlDict.endTime;

if currentTime<endTime
    proceed = true;
else
    proceed = false;
end