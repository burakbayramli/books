function cfdSetupRegion
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes the data base in which the variables will be
%   stored
%--------------------------------------------------------------------------

global Region;

Region.caseDirectoryPath = pwd;
Region.STEADY_STATE_RUN = true;
tic;
