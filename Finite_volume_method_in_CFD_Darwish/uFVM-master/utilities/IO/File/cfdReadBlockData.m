function blockData = cfdReadBlockData(blockName, fileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function read block content in a FOAM file
%--------------------------------------------------------------------------
%
% Read File
fileID = fopen(fileDirectory, 'r');

% Skip Header
for i=1:16
    fgetl(fileID);
end

blockExists = false;
iEntry = 1;
blockData = {};
while(~feof(fileID))
    
    % Read each line
    tline = fgetl(fileID);
    
    % Skip empty lines
    if isempty(tline)
        continue;
    end   
    
    % Skip empty lines
    if length(tline)>1
        if strcmp(tline(1:2), '//')
            continue;
        end
    end
    
    C = textscan(tline, '%s', 1);
    if isempty(C{1})
        continue;
    end
    
    if(strcmp(C{1}{1}, blockName))
        blockExists = true;
        
        % Skip to content
        tline = fgetl(fileID);
        tline = fgetl(fileID);
        
        str = textscan(tline, '%s', 1);
        
        % Search for entries until reaching the closing bracket of block
        while ~strcmp(str{1}{1}, '}')
            % Skip empty lines
            if isempty(tline)
                tline = fgetl(fileID);
                str = textscan(tline, '%s', 1);
                continue;
            end
            
            % Collect key and value
            C = textscan(tline, '%s %[^\n]', 1);            
            key = C{1}{1};
            if isempty(C{2})
                tline = fgetl(fileID);
                str = textscan(tline, '%s', 1);                
                continue;
            end
            value = C{2}{1}(1:end-1);
            
            % Store key and value in blockData
            blockData{iEntry, 1} = key;
            blockData{iEntry, 2} = value;
            
            % Update iter and get the next non-empty line
            iEntry = iEntry + 1;            
            tline = fgetl(fileID);
            while isempty(strrep(tline, ' ', ''))
                tline = fgetl(fileID);
            end
            str = textscan(tline, '%s', 1);
        end
        break;
    end    
end

fclose(fileID);

if ~blockExists
    blockData = {};
end

