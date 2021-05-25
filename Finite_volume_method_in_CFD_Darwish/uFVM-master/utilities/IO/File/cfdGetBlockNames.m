function blockNames = cfdGetBlockNames(fileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the names of the blocks available in the file
%--------------------------------------------------------------------------
%
% Read File
fileID = fopen(fileDirectory, 'r');

% Skip Header
for i=1:16
    fgetl(fileID);
end

iBlockName = 1;
while(~feof(fileID)) 
    % Read each line
    tline = fgetl(fileID);
    
    % Skip empty lines
    if isempty(tline)
        continue;
    end
    
    % Skip commented lines
    if length(tline)>1
        if strcmp(tline(1:2), '//')
            continue;
        end
    end   
    
    C = textscan(tline, '%s %s', 1);
    if ~isempty(C{1}) && isempty(C{2})        
        entryName = C{1}{1};                
        C = textscan(fileID, '%s', 1);
        if strcmp(C{1}{1}, '{')
            blockNames{iBlockName} = entryName;
            iBlockName = iBlockName + 1;
        end                
    end    
end






