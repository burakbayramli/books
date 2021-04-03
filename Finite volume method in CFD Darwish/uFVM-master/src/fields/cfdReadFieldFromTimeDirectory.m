function theMeshField = cfdReadFieldFromTimeDirectory(theFieldName, theTimeDirectory)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads fields from time directory
%--------------------------------------------------------------------------

% Case directory
caseDirectoryPath = cfdGetCaseDirectoryPath;

% Get mesh
theMesh = cfdGetMesh;
theNumberOfElements = cfdGetNumberOfElements;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;

fieldFilePath = [caseDirectoryPath, filesep, num2str(theTimeDirectory), filesep, theFieldName];

% Open field in read mode
fid = fopen(fieldFilePath, 'r');

if fid<0
    error('File %s not available', theFieldName);
end

% Initialize header
header = cell(0);

% Scan/Read header
while ~feof(fid)
    tline = fgetl(fid);
    
    % Skip empty lines
    tline = cfdSkipEmptyLines(fid, tline);
    
    % Skip macro-commented section
    tline = cfdSkipMacroComments(fid, tline);
    
    % Skip commented lines
    tline = cfdSkipCommentedLine(fid, tline);
    
    % read header block
    if cfdContains(tline, 'FoamFile')
        if isempty(header)
            header = cfdReadCfdDictionary(fid, 'FoamFile');
        else
            break;
        end
    else
        if ~isempty(header)
            break;
        end
    end
end

% Initialize and store field info
theMeshField.name = theFieldName;

% Field type
theMeshField.type = header.class;

if strcmp(theMeshField.type, 'volScalarField')
    theInteriorArraySize = theMesh.numberOfElements;
    theBoundaryArraySize = theMesh.numberOfBElements;    
    theMeshField.phi     = cfdScalarList(theInteriorArraySize+theBoundaryArraySize);   
elseif strcmp(theMeshField.type, 'volVectorField')
    theInteriorArraySize = theMesh.numberOfElements;
    theBoundaryArraySize = theMesh.numberOfBElements;    
    theMeshField.phi     = cfdVectorList(theInteriorArraySize+theBoundaryArraySize);       
elseif strcmp(theMeshField.type, 'surfaceScalarField')
    theInteriorArraySize = theMesh.numberOfInteriorFaces;
    theBoundaryArraySize = theMesh.numberOfBFaces;    
    theMeshField.phi     = cfdScalarList(theInteriorArraySize+theBoundaryArraySize);    
elseif strcmp(theMeshField.type, 'surfaceVector3Field')    
    theInteriorArraySize = theMesh.numberOfInteriorFaces;
    theBoundaryArraySize = theMesh.numberOfBFaces; 
    theMeshField.phi     = cfdVectorList(theInteriorArraySize+theBoundaryArraySize);       
end

% Read dimensions
theMeshField.dimensions = cfdGetKeyValue('dimensions', 'dimensions', fid);

% Read and store internal field
internalField = cfdGetKeyValue('internalField', 'string', fid);
C = textscan(internalField, '%s', 1);
valueType = C{1}{1};

if strcmp(theMeshField.type, 'surfaceScalarField')
    
    if strcmp(valueType, 'uniform')
        value_str = textscan(internalField, 'uniform %f;', 1);
        theMeshField.phi(1:theNumberOfInteriorFaces) = value_str{1};
    elseif strcmp(valueType, 'nonuniform')
        theMeshField.phi(1:theNumberOfInteriorFaces) = cfdReadNonuniformList('internalField', fieldFilePath);
    end
    
    % Read and store cfdBoundary field
    boundaries = theMesh.cfdBoundaryPatchesArray;
    theNumberOfBPatches = length(boundaries);
    
    for iBPatch=1:theNumberOfBPatches
        
        % Get info
        theBCInfo = boundaries{iBPatch};
        iFaceStart = theBCInfo.startFaceIndex;
        iFaceEnd = iFaceStart+theBCInfo.numberOfBFaces-1;
        
        
        % Get field cfdBoundary condition
        type = cfdGetKeyValueFromBlock('type', ['boundaryField/', theBCInfo.name], fieldFilePath);
        
        % Store field cfdBoundary condition
        boundaryPatchRef.type = type;
        
        % Read cfdBoundary value
        value = cfdGetKeyValueFromBlock('value', ['boundaryField/', theBCInfo.name], fieldFilePath);
        
        if isempty(value)
            boundaryPatchRef.value = 0;
            
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
            
            % Clear boundaryPatchRef
            structureFields = fieldnames(boundaryPatchRef);
            for structureFieldName=structureFields
                boundaryPatchRef = rmfield(boundaryPatchRef, structureFieldName);
            end
            
            continue;
        end
        
        C = textscan(value, '%s', 1);
        valueType = C{1}{1};
        
        boundaryPatchRef.valueType = valueType;
        
        if strcmp(valueType, 'uniform')
            value_str = textscan(value, 'uniform %f', 1);
            theMeshField.phi(iFaceStart:iFaceEnd) = value_str{1};
            boundaryPatchRef.value = value_str{1};
        elseif strcmp(valueType, 'nonuniform')
            valueList = cfdReadNonuniformList(theBCInfo.name, fieldFilePath);
            theMeshField.phi(iFaceStart:iFaceEnd) = valueList;
            boundaryPatchRef.value = valueList;
        end
        
        theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        
    end
    
else
    
    if strcmp(valueType, 'uniform')
        if strcmp(theMeshField.type, 'volScalarField')
            value_str = textscan(internalField, 'uniform %f;', 1);
            theMeshField.phi(1:theNumberOfElements) = value_str{1};
        elseif strcmp(theMeshField.type, 'volVectorField')
            value_str = textscan(internalField, 'uniform (%f %f %f);', 1);
            for iElement=1:theNumberOfElements
                theMeshField.phi(iElement,:) = [value_str{1}, value_str{2}, value_str{3}];
            end
        end
    elseif strcmp(valueType, 'nonuniform')
        theMeshField.phi(1:theNumberOfElements, :) = cfdReadNonuniformList('internalField', fieldFilePath);
    end
    
    % Read and store cfdBoundary field
    boundaries = theMesh.cfdBoundaryPatchesArray;
    theNumberOfBPatches = length(boundaries);
    
    for iBPatch=1:theNumberOfBPatches
        
        % Get info
        theBCInfo = boundaries{iBPatch};
        
        numberOfBFaces = theBCInfo.numberOfBFaces;
        iFaceStart = theBCInfo.startFaceIndex;
        
        iElementStart = theNumberOfElements+iFaceStart-theNumberOfInteriorFaces;
        iElementEnd = iElementStart+numberOfBFaces-1;
        
        
        % Get field cfdBoundary condition
        type = cfdGetKeyValueFromBlock('type', ['boundaryField/', theBCInfo.name], fieldFilePath);
        
        % Store field cfdBoundary condition
        boundaryPatchRef.type = type;
        
        % Read cfdBoundary value
        value = cfdGetKeyValueFromBlock('value', ['boundaryField/', theBCInfo.name], fieldFilePath);
        
        if isempty(value)
            if strcmp(theMeshField.type, 'volScalarField') || strcmp(theMeshField.type, 'surfaceScalarField')
                boundaryPatchRef.value = 0;
            elseif strcmp(theMeshField.type, 'volVectorField')
                boundaryPatchRef.value = [0,0,0];
            end
            
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
            
            % Clear boundaryPatchRef
            structureFields = fieldnames(boundaryPatchRef);
            for structureFieldName=structureFields
                boundaryPatchRef = rmfield(boundaryPatchRef, structureFieldName);
            end
            
            continue;
        end
        
        C = textscan(value, '%s', 1);
        valueType = C{1}{1};
        
        boundaryPatchRef.valueType = valueType;
        
        if strcmp(valueType, 'uniform')
            if strcmp(theMeshField.type, 'volScalarField') || strcmp(theMeshField.type, 'surfaceScalarField')
                value_str = textscan(value, 'uniform %f', 1);
                theMeshField.phi(iElementStart:iElementEnd) = value_str{1};
                boundaryPatchRef.value = value_str{1};
            elseif strcmp(theMeshField.type, 'volVectorField')
                value_str = textscan(value, 'uniform (%f %f %f)', 1);
                for iBElement=iElementStart:iElementEnd
                    theMeshField.phi(iBElement,:) = [value_str{1}, value_str{2}, value_str{3}];
                end
                boundaryPatchRef.value = [value_str{1}, value_str{2}, value_str{3}];
            end
        elseif strcmp(valueType, 'nonuniform')
            valueList = cfdReadNonuniformList(theBCInfo.name, fieldFilePath);
            theMeshField.phi(iElementStart:iElementEnd, :) = valueList;
            boundaryPatchRef.value = valueList;
        end
        
        theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        
    end
    
    % Clear boundaryPatchRef
    structureFields = fieldnames(boundaryPatchRef);
    for structureFieldName=structureFields
        boundaryPatchRef = rmfield(boundaryPatchRef, structureFieldName);
    end
end

% Close file
fclose(fid);
