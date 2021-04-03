function state = cfdIsWithBuoyancy
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Return if case includes buoyancy
%--------------------------------------------------------------------------

global Region;

state = Region.buoyancy;