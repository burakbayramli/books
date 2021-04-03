function cfdReadFacesFile(facesFileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads faces file in constant/polyMesh
%--------------------------------------------------------------------------

% Open faces file in read mode
ffid = fopen(facesFileDirectory, 'r');

% Initialize header
header = cell(0);

% Scan/Read header
while ~feof(ffid)         
    tline = fgetl(ffid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(ffid, tline);

    % Skip macro-commented section
    tline = cfdSkipMacroComments(ffid, tline);
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(ffid, tline);    

    % read header block
    if cfdContains(tline, 'FoamFile')   
        if isempty(header)
            header = cfdReadCfdDictionary(ffid, 'FoamFile');
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
tline = cfdSkipEmptyLines(ffid, tline);  

% Skip to number of faces        
C = textscan(tline,'%d',1);
while isempty(C{1})        
    tline = fgetl(ffid);
    if isempty(tline)
        continue;
    end

    if feof(ffid)
       error('faces list not there!!'); 
    end
    C = textscan(tline,'%d',1);
end    
numberOfFaces = C{1};

% Initialize
faceNodes = cfdLabelListList(numberOfFaces);    

% Reach list opening paranthesis
c = fscanf(ffid, '%c', 1);  
while ~strcmp(c, '(')
    c = fscanf(ffid, '%c', 1);  
end

% Read items in list
for item=1:numberOfFaces
    numberOfPoints = fscanf(ffid, '%d', 1);
    
    % Reach sub list opening paranthesis
    c = fscanf(ffid, '%c', 1);  
    while ~strcmp(c, '(')
        c = fscanf(ffid, '%c', 1);  
    end    
    
    for iPoint=1:numberOfPoints
        faceNodes{item} = [faceNodes{item} fscanf(ffid, '%d', 1) + 1];
    end
    
    % Reach sub list closing paranthesis
    c = fscanf(ffid, '%c', 1);  
    while ~strcmp(c, ')')
        c = fscanf(ffid, '%c', 1);  
    end      
end

% Close
fclose(ffid);

% Save and Store
mesh = cfdGetMesh;
%
mesh.faceNodes = faceNodes;
mesh.numberOfFaces = numberOfFaces;

cfdSetMesh(mesh);