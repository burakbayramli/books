function cfdReadBoundaryFile(cfdBoundaryFileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads cfdBoundary file in constant/polyMesh
%--------------------------------------------------------------------------

% Open neighbour file in read mode
fbid = fopen(cfdBoundaryFileDirectory, 'r');

% Initialize header
header = cell(0);

% Scan/Read header
while ~feof(fbid)         
    tline = fgetl(fbid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fbid, tline);

    % Skip macro-commented section
    tline = cfdSkipMacroComments(fbid, tline);
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(fbid, tline);    

    % read header block
    if cfdContains(tline, 'FoamFile')   
        if isempty(header)
            header = cfdReadCfdDictionary(fbid, 'FoamFile');
        else
            break;
        end
    else
        if ~isempty(header)
            break;
        end        
    end
end

% Skip to number of points 
tline = cfdSkipEmptyLines(fbid, tline);

C = textscan(tline,'%d',1);
while isempty(C{1})        
    tline = fgetl(fbid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fbid, tline);

    if feof(fbid)
       error('cfdBoundary list not there!!'); 
    end
    C = textscan(tline,'%d',1);
end    
numberOfBoundaries = C{1}; 

% Initialize cfdBoundary array
cfdBoundaries = cell(numberOfBoundaries, 1);

% Reach list opening paranthesis
c = fscanf(fbid, '%c', 1);  
while ~strcmp(c, '(')
    c = fscanf(fbid, '%c', 1);  
end

% Read cfdBoundary info
for item=1:numberOfBoundaries
    cfdBoundary.name = fscanf(fbid, '%s', 1);
    cfdBoundary.index = item;
    
    cfdBoundaryDictionary = cfdReadCfdDictionary(fbid, cfdBoundary.name);
    fieldNames = fieldnames(cfdBoundaryDictionary);
    
    for iField=1:length(fieldNames)
        fieldName = fieldNames{iField};
        
        if strcmp(fieldName, 'nFaces')
            cfdBoundary.numberOfBFaces = str2double(cfdBoundaryDictionary.(fieldName));
        elseif strcmp(fieldName, 'startFace')
            cfdBoundary.startFaceIndex = 1+str2double(cfdBoundaryDictionary.(fieldName));            
        else
            cfdBoundary.(fieldName) = cfdBoundaryDictionary.(fieldName);
        end                
    end
    cfdBoundaries{item} = cfdBoundary;
end

% Close
fclose(fbid);

% Save and store
mesh = cfdGetMesh;
%
mesh.cfdBoundaryPatchesArray    = cfdBoundaries;
mesh.numberOfBoundaryPatches = numberOfBoundaries;
%
cfdSetMesh(mesh);