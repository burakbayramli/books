function tline = cfdSkipMacroComments(fileID, varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function skips empty lines in a fileID
%--------------------------------------------------------------------------

if isempty(varargin)
    tline = fgetl(fileID);
else
    tline = varargin{1};
end

trimmedTline = strtrim(tline);
if strcmp(trimmedTline(1:2), '/*')        
    while ~cfdContains(tline, '*/')
        tline = fgetl(fileID);
    end    
    tline = fgetl(fileID);
end