function state = cfdIsTimeDirectory(theDirectoryPath)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if the directory provided is a time directory as
%   in FOAM standard
%--------------------------------------------------------------------------

% Check if the directory has a numeric name
C = textscan(theDirectoryPath, '%f');
if isempty(C{1})
    state = false;
    return;
end

% Check if directory contains any of the field files
theFieldNames = cfdGetFields;
files = dir(theDirectoryPath);
state = false;
for iFile=1:length(files)
    fileName = files(iFile).name;
    if ~isempty(find(strcmp(theFieldNames,fileName)))
        state = true;
        return;
    end
end