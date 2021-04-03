function deltaT = cfdGetDeltaT
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns deltaT from data base
% -------------------------------------------------------------------------

global Region;

deltaT = Region.foamDictionary.controlDict.deltaT;