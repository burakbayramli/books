function cfdReadNeighbourFile(neighbourFileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads neighbour file in constant/polyMesh
%--------------------------------------------------------------------------

% Open neighbour file in read mode
fnid = fopen(neighbourFileDirectory, 'r');

% Initialize header
header = cell(0);

% Scan/Read header
while ~feof(fnid)         
    tline = fgetl(fnid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fnid, tline);

    % Skip macro-commented section
    tline = cfdSkipMacroComments(fnid, tline);
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(fnid, tline);    

    % read header block
    if cfdContains(tline, 'FoamFile')   
        if isempty(header)
            header = cfdReadCfdDictionary(fnid, 'FoamFile');
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
tline = cfdSkipEmptyLines(fnid, tline);

C = textscan(tline,'%d',1);
while isempty(C{1})        
    tline = fgetl(fnid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fnid, tline);

    if feof(fnid)
       error('owner list not there!!'); 
    end
    C = textscan(tline,'%d',1);
end    
numberOfNeighbours = C{1}; 

% Initialize
neighbours = cfdLabelList(numberOfNeighbours);

% Reach list opening paranthesis
c = fscanf(fnid, '%c', 1);  
while ~strcmp(c, '(')
    c = fscanf(fnid, '%c', 1);  
end

% Read items in list
for n=1:numberOfNeighbours
    neighbours(n) = fscanf(fnid, '%d', 1) + 1;      
end

% Close
fclose(fnid);

% Save and store
mesh = cfdGetMesh;
%
mesh.numberOfInteriorFaces = numberOfNeighbours;
mesh.numberOfBFaces        = mesh.numberOfFaces - numberOfNeighbours;
mesh.neighbours            = neighbours;
mesh.numberOfElements      = max(neighbours);
mesh.numberOfBElements     = mesh.numberOfFaces - numberOfNeighbours;
%
cfdSetMesh(mesh);