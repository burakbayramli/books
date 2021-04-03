function tline = cfdSkipCommentedLine(fileID, varargin)
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
if length(tline)<2
    return;
end

while strcmp(trimmedTline(1:2), '//') 
    tline = fgetl(fileID);
    if ~isempty(tline)
        trimmedTline = strtrim(tline);
    else
        break;
    end
end