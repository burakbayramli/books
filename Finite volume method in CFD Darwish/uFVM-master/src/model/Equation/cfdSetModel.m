function cfdSetModel(theModel)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function sets up the model
%--------------------------------------------------------------------------
%

global Region;

Region.model.(theModel.name) = theModel;