function timeSteps = cfdGetTimeSteps
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function collects the time steps that exist in the current
%   directory
%--------------------------------------------------------------------------

files = dir;
i = 1;
for iFile=1:length(files)
    fileName = files(iFile).name;
    if files(iFile).isdir && ~strcmp(fileName, '.') && ~strcmp(fileName, '..')
        if ~strcmp(fileName, 'constant') && ~strcmp(fileName, 'system') && ~strcmp(fileName, 'convergence')
            if ~cfdIsTimeDirectory(fileName)
                continue;
            end
            timeSteps(i, 1) = eval(fileName);
            i = i + 1;
        end
    end
end

timeSteps = sort(timeSteps, 1, 'ascend');