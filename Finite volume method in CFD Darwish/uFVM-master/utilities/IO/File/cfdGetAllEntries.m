function entries = cfdGetAllEntries(fileID)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads they value of all entries in a FOAM file
%--------------------------------------------------------------------------

% resrt and skip header
frewind(fileID);
tline = cfdSkipHeader(fileID);

% Initialize
entries = cell(1);

% Loop and store
iEntry = 1;
while ~feof(fileID)    
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(fileID, tline);
    
    % Split if more than entry on the same line
    C = strsplit(tline, ';');
    
    for iInlineEntry=1:length(C)
        if isempty(C{iInlineEntry})
            continue;
        end
        
        entry = textscan(tline, '%s', 1);
        key = entry{1}{1};
        
        start_key = strfind(C{iInlineEntry}, key);
        value = strtrim(C{iInlineEntry}(start_key+length(key):end));
        
        entries{iEntry}.key   = key;
        entries{iEntry}.value = value;
        
        iEntry = iEntry + 1;
    end 
    
    tline = fgetl(fileID);
end
