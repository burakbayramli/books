function tline = cfdSkipHeader(fileID)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function skips the header
%--------------------------------------------------------------------------

while ~feof(fileID)
    tline = fgetl(fileID);
    tline = cfdSkipMacroComments(fileID, tline);
    tline = cfdSkipEmptyLines(fileID, tline);
    [header, tline] = cfdReadCfdDictionary(fileID, 'FoamFile');
    tline = fgetl(fileID);
    tline = cfdSkipCommentedLine(fileID, tline);
    tline = cfdSkipEmptyLines(fileID, tline);
    break;
end