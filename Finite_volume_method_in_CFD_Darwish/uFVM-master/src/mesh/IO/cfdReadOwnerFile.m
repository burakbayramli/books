function cfdReadOwnerFile(ownerFileDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads owner file in constant/polyMesh
%--------------------------------------------------------------------------

% Read points file in read mode
foid = fopen(ownerFileDirectory, 'r');

% Initialize header
header = cell(0);

% Scan/Read header
while ~feof(foid)         
    tline = fgetl(foid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(foid, tline);

    % Skip macro-commented section
    tline = cfdSkipMacroComments(foid, tline);
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(foid, tline);    

    % read header block
    if cfdContains(tline, 'FoamFile')   
        if isempty(header)
            header = cfdReadCfdDictionary(foid, 'FoamFile');
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
tline = cfdSkipEmptyLines(foid, tline);

C = textscan(tline,'%d',1);
while isempty(C{1})        
    tline = fgetl(foid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(foid, tline);

    if feof(foid)
       error('owner list not there!!'); 
    end
    C = textscan(tline,'%d',1);
end    
numberOfOwners = C{1}; 

% Initialize
owners = cfdLabelList(numberOfOwners);

% Reach list opening paranthesis
c = fscanf(foid, '%c', 1);  
while ~strcmp(c, '(')
    c = fscanf(foid, '%c', 1);  
end

% Read items in list
for n=1:numberOfOwners
    owners(n) = fscanf(foid, '%d', 1) + 1;      
end

% Close
fclose(foid);

% Save and store
mesh = cfdGetMesh;
%
mesh.numberOfFaces = numberOfOwners;
mesh.owners        = owners;
%
cfdSetMesh(mesh);