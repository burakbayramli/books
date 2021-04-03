function state = cfdIsFileExists(fileName, varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function writes the results at each write interval
%--------------------------------------------------------------------------

if isempty(varargin)
    files = dir;    
else
    files = dir(varargin{1});    
end

state = false;
for iFile=1:length(files)
    if ~files(iFile).isdir
        if strcmp(fileName, files(iFile).name)
            state = true;
            break;
        end
    end
end