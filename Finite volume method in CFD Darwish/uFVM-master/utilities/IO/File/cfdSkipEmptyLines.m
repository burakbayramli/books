function tline = cfdSkipEmptyLines(fileID, varargin)
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

while isempty(tline)
    tline = fgetl(fileID);
end