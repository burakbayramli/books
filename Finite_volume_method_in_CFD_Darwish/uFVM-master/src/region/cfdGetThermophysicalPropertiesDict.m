function theThermophysicalProperties = cfdGetThermophysicalPropertiesDict
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a the thermophysicalProperties dictionary from
%   the data base
%--------------------------------------------------------------------------

global Region;

theThermophysicalProperties = Region.foamDictionary.thermophysicalProperties;
