function cfdClearResults
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function deletes the folders and files that have been generated
%   through the run
%--------------------------------------------------------------------------

% Clear time steps
theTimeSteps = cfdGetTimeSteps;
for timeStep=theTimeSteps'
   if timeStep~=0 
       if cfdIsFolderExists(num2str(timeStep))
           rmdir(num2str(timeStep), 's');
       end
   end
end

% Clear other additional folders
if cfdIsFolderExists('convergence')
    rmdir('convergence', 's');
end