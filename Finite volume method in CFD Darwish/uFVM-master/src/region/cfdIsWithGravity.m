function state = cfdIsWithGravity
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Return if case includes gravity
%--------------------------------------------------------------------------

global Region;

state = isfield(Region.foamDictionary, 'g');