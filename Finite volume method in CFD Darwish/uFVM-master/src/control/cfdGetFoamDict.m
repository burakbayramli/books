function theFoamDict = cfdGetFoamDict
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a model from data base
%--------------------------------------------------------------------------

global Region;

theFoamDict = Region.foamDictionary;
