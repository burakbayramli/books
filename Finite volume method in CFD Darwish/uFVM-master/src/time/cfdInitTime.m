function cfdInitTime
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

global Region;

if strcmp(Region.foamDictionary.controlDict.startFrom, 'startTime')
    Region.time.currentTime = Region.foamDictionary.controlDict.startTime;
elseif strcmp(Region.foamDictionary.controlDict.startFrom, 'latestTime')
    timeSteps = cfdGetTimeSteps;
    Region.time.currentTime = max(timeSteps); 
elseif strcmp(Region.foamDictionary.controlDict.startFrom, 'firstTime')
    timeSteps = cfdGetTimeSteps;
    Region.time.currentTime = min(timeSteps);     
end