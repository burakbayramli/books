function [dictionary, tline] = cfdReadCfdDictionary(fileID, dictionaryName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads a dictionary which is defined as a title in
%   addition to a block of keys and values. This function has to be
%   provided with a file stream (an opened file).
%--------------------------------------------------------------------------

% Reset position
frewind(fileID);

% Initialize dictionary
dictionary = struct;

% Reach dictionary name
tline = fgetl(fileID);
while ~cfdContains(tline, dictionaryName)    
    if feof(fileID)
        error('%s not found', dictionaryName);
    end        
    tline = fgetl(fileID);    
end

% Ensure that the found key is not a key with same name under other
% dictionary
C = textscan(tline, '%s%s');

if ~cfdContains(C{2},'{') && length(strtrim(tline))~=length(dictionaryName)
    % Reach dictionary name
    tline = fgetl(fileID);
    while ~cfdContains(tline, dictionaryName)    
        if feof(fileID)
            error('%s not found', dictionaryName);
        end        
        tline = fgetl(fileID);    
    end    
end

% Concatenate all block in single string
while ~cfdContains(tline, '{')
    if feof(fileID)
        error('%s is incomplete', dictionaryName);
    end      
    tline = fgetl(fileID);
end

openingBraceletColIndex = strfind(tline, '{');

if length(openingBraceletColIndex)>1
    openingBraceletColIndex = openingBraceletColIndex(1);
end

if openingBraceletColIndex==length(tline)
    tline = fgetl(fileID);  
    openingBraceletColIndex = 0;
end

block_str = [];

if cfdContains(tline, '}')
    endingBraceletColIndex = strfind(tline, '}');
    block_str = tline(openingBraceletColIndex+1:endingBraceletColIndex-1);
else
    while ~cfdContains(tline, '}')
        block_str = [block_str tline(openingBraceletColIndex+1:end)];
        openingBraceletColIndex = 0;
        tline = fgetl(fileID);
    end

    endingBraceletColIndex = strfind(tline, '}');
    if length(endingBraceletColIndex)>1
        endingBraceletColIndex = endingBraceletColIndex(1);
    end
    
    if length(tline)~=1
        block_str = [block_str tline(1:endingBraceletColIndex-1)];        
    end        
end

% Read contents
C = strsplit(block_str, ';');
for i=1:length(C)
    if isempty(strtrim(C{i}))
        continue;
    end
    
    % Get key
    key = textscan(C{i}, '%s', 1); key = key{1}{1};  
    key_start = strfind(C{i}, key); 
    if length(key_start)>1
        key_start = key_start(1);
    end
    
    % Get value
    value = strtrim(C{i}((key_start+length(key)):end));
    
    % Store
    dictionary.(key) = value;
end

