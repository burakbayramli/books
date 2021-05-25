function foamDict = cfdReadFoamDictFile(foamDictFileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads a foam dict file
%--------------------------------------------------------------------------

% Read points file in read mode
fid = fopen(foamDictFileDirectory, 'r');

% Initialize header
header = cell(0);

% Scan/Read header
while ~feof(fid)
    tline = fgetl(fid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fid, tline);
    
    % Skip macro-commented section
    tline = cfdSkipMacroComments(fid, tline);
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(fid, tline);
    
    % read header block
    if cfdContains(tline, 'FoamFile')
        if isempty(header)
            header = cfdReadCfdDictionary(fid, 'FoamFile');
        else
            break;
        end
    else
        if ~isempty(header)
            break;
        end
    end
end

% Concatenate all block in single string
block_str = [];
while ~feof(fid)
    tline = fgetl(fid);    
    if isempty(tline)
        continue;
    end
    
    if length(tline)>1
        if strcmp(strtrim(tline(1:2)),'//')
            continue;
        end
    end
    
    block_str = [block_str tline];
end

% Make all dictionary as single block
block_str = ['{',block_str,'}'];

% Initialize foamDict
foamDict = cfdConvertStrBlockToDict(block_str);