function theCaseDirectory = cfdGetCaseDirectoryPath
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the case directory path
%--------------------------------------------------------------------------

global Region;

theCaseDirectory = Region.caseDirectoryPath;