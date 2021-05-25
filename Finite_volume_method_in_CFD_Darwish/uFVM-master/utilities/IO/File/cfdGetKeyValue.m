function value = cfdGetKeyValue(key, valueType, fileID)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads they value of any entry (key) in a FOAM file
%--------------------------------------------------------------------------

frewind(fileID);

value = 'NA';

while ~feof(fileID)
    tline = fgetl(fileID);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fileID, tline);
    
    if cfdContains(tline, key)
        splittedTline = strsplit(tline, ';');
        for iEntry=1:length(splittedTline)
            if isempty(strtrim(splittedTline{iEntry}))
                continue;
            end
            if cfdContains(splittedTline{iEntry}, key)
                
                % Check if the found pattern is a key (not a string value
                % which has the same name of the key)
                C = textscan(splittedTline{iEntry}, '%s', 1);                
                if ~strcmp(C{1}, key)
                    continue;
                end
                
                key_start = strfind(splittedTline{iEntry}, key);
                value = strtrim(splittedTline{iEntry}(key_start+length(key):end));
                
                if strcmp(valueType, 'scalar') || strcmp(valueType, 'label')
                    value = str2double(value);
                elseif strcmp(valueType, 'cfdLabelList')
                    C = textscan(value, '(%d %d %d)');
                    value = [C{1}, C{2}, C{3}];
                elseif strcmp(valueType, 'cfdScalarList')                    
                    C = textscan(value, '(%f %f %f)');
                    value = [C{1}, C{2}, C{3}];
                elseif strcmp(valueType, 'dimensions')
                    C = textscan(value, '[%d%d%d%d%d%d%d]');
                    value = [C{1}, C{2}, C{3}, C{4}, C{5}, C{6}, C{7}];                  
                end
                
                break;
            end
        end
        
        if ~strcmp(value, 'NA')
            break;
        end
    end
end
