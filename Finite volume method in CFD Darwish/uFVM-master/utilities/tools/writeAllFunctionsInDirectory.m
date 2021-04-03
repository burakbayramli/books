function cfdWriteAllFunctionsInDirectory(directory, logFileName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function writes all the paths of the functions which exist in the
%   current directory and sub-directories
%--------------------------------------------------------------------------


% Check system
if isunix
    system(['find ', directory, ' -iname ''*.m'' > ', logFileName]);
elseif ispc
    
elseif ismac
    
end