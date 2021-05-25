function blockData = cfdReadBlock(blockName, fileDirectory, varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads they value of any entry (key) in a FOAM file given
%   the block name
%--------------------------------------------------------------------------

% Read File
fileID = fopen(fileDirectory, 'r');

if nargin==2
    cfdSkipHeader = true;
else
    cfdSkipHeader = varargin{1};
end

% Skip Header
if cfdSkipHeader
    for i=1:16
        fgetl(fileID);
    end
end

% Check if required block is a subblock
nSubBlocks = length(strfind(blockName, '/'));
if nSubBlocks>0
    blockNames = textscan(blockName, '%s', nSubBlocks+1, 'delimiter', '/');
    blockNames = blockNames{1};
else
    blockNames = {blockName};
end

blockExists = false;
iEntry = 1;

while(~feof(fileID))
    % Read each line
    tline = fgetl(fileID);
    
    % Skip empty lines
    if isempty(tline)
        continue;
    end
    if isempty(strrep(tline, ' ', ''))
        continue;
    end    
    
    % Skip commented lines
    if length(tline)>1
        if strcmp(tline(1:2), '//')
            continue;
        end
    end
    
    C = textscan(tline, '%s', 1);
    for iBlock=1:nSubBlocks+1
        blockName = blockNames{iBlock};
        while ~strcmp(C{1}{1}, blockName)
            tline = fgetl(fileID);
            
            if tline==-1
                blockData = {};
                return; 
            end
            
            % Skip empty lines
            if isempty(tline)
                continue;
            end
            
            % Skip empty lines
            if isempty(strrep(tline, ' ', ''))
                continue;
            end
            
            % Skip commented lines
            if length(tline)>1
                if strcmp(tline(1:2), '//')
                    continue;
                end
            end            
            
            C = textscan(tline, '%s', 1);
            if strcmp(C{1}{1}, blockNames{end})
                
                blockExists = true;
                
                % Skip to content
                tline = fgetl(fileID);
                tline = fgetl(fileID);
                
                str = textscan(tline, '%s', 1);
                
                % Search for entries until reaching the closing bracket of block
                while ~strcmp(str{1}{1}, '}')
                    % Skip empty lines
                    if isempty(strrep(tline, ' ', ''))
                        tline = fgetl(fileID);
                        str = textscan(tline, '%s', 1);
                        continue;
                    end
                    
                    % Collect key and value
                    C = textscan(tline, '%s %[^\n]', 1);
                    key = strtrim(C{1}{1});
                    value = strtrim(C{2}{1});
                    value = value(1:end-1);
                    
                    % Store key and value in blockData
                    blockData{iEntry, 1} = key;
                    blockData{iEntry, 2} = value;
                    
                    % Update iter and get the next non-empty line
                    iEntry = iEntry + 1;
                    tline = fgetl(fileID);
                    while isempty(tline)
                        tline = fgetl(fileID);
                    end
                    str = textscan(tline, '%s', 1);
                end
                break;
            end
        end
    end
    break;
end

fclose(fileID);

if ~blockExists
    blockData = {};
end

