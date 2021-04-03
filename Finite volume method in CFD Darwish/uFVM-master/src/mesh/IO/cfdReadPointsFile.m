function cfdReadPointsFile(pointsFileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads points file in constant/polyMesh
%--------------------------------------------------------------------------

% Read points file in read mode
fpid = fopen(pointsFileDirectory, 'r');

% Initialize header
header = cell(0);

% Scan/Read header
while ~feof(fpid)         
    tline = fgetl(fpid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fpid, tline);

    % Skip macro-commented section
    tline = cfdSkipMacroComments(fpid, tline);
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(fpid, tline);    

    % read header block
    if cfdContains(tline, 'FoamFile')   
        if isempty(header)
            header = cfdReadCfdDictionary(fpid, 'FoamFile');
        else
            break;
        end
    else
        if ~isempty(header)
            break;
        end        
    end
end

% Read number of points
numberOfPoints = fscanf(fpid, '%d', 1);   

% Initialize
nodeCentroids = cfdVectorList(numberOfPoints, cfdVector(0,0,0));

% Reach list opening paranthesis
c = fscanf(fpid, '%c', 1);  
while ~strcmp(c, '(')
    c = fscanf(fpid, '%c', 1);  
end

% Read items in list
for item=1:numberOfPoints      
    % Reach cfdVector opening paranthesis
    c = fscanf(fpid, '%c', 1);  
    while ~strcmp(c, '(')
        c = fscanf(fpid, '%c', 1);  
    end

    % Read coordinates
    x = fscanf(fpid, '%f', 1);
    y = fscanf(fpid, '%f', 1);
    z = fscanf(fpid, '%f', 1);
    
    % Store in 
    nodeCentroids(item,:) = cfdVector(x, y, z);
    
    % Reach cfdVector closing paranthesis
    c = fscanf(fpid, '%c', 1);  
    while ~strcmp(c, ')')
        c = fscanf(fpid, '%c', 1);  
    end     
end

% Close
fclose(fpid);

% Save and Store
mesh = cfdGetMesh;
%
mesh.nodeCentroids = nodeCentroids;
mesh.numberOfNodes = numberOfPoints;
%
cfdSetMesh(mesh);