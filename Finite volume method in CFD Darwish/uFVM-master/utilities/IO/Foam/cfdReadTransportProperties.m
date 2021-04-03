function cfdReadTransportProperties
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads cfdTransport properties file
%--------------------------------------------------------------------------

global Region;

fprintf('\nReading Transport Properties ...');

caseDirectoryPath = cfdGetCaseDirectoryPath;

transportPropertiesFile = [caseDirectoryPath, filesep, 'constant', filesep, 'transportProperties'];

% Check if "transportProperties" exists
if exist(transportPropertiesFile, 'file')~=2
    return;
end

% Open transportProperties file in read mode
fid = fopen(transportPropertiesFile, 'r');

% Collect constant cfdTransport properties from file
properties = cfdGetAllEntries(fid);

% Get mesh
theMesh = cfdGetMesh;

% Store attributes in the foam data base and create property fields
for iProperty=1:length(properties)
    
    propertyName = properties{iProperty}.key;
    
    % Skip if the entry is the transport model type
    if strcmp(propertyName, 'transportModel')
        continue;
    end
    
    fprintf(['\nReading ', propertyName,' ...']);
    
    duplicateIndex = strfind(properties{iProperty}.value, properties{iProperty}.key);
    if ~isempty(duplicateIndex)
        properties{iProperty}.value(duplicateIndex:duplicateIndex+length(properties{iProperty}.key)-1) = '';
    end
    
    C = textscan(properties{iProperty}.value, '[%d %d %d %d %d %d %d] %f');
    
    % Value
    propertyValue = C{8};
    if isempty(propertyValue)
        error('%s does not have a value\n');
    end
    
    % Dimensions
    propertyDimensions = [C{1} C{2} C{3} C{4} C{5} C{6} C{7}];
    if isempty(propertyDimensions)
        error('%s does not have dimensions\n');
    end
    
    % Setup mesh field
    cfdSetupMeshField(propertyName, 'volScalarField');
    
    % Store initial value in mesh field
    theMeshField = cfdGetMeshField(propertyName);
    theMeshField.name = propertyName;
    theMeshField.dimensions = propertyDimensions;
    theMeshField.phi(:) = propertyValue;
    
    % Set zeroGradient bc type for all patches
    for iBPatch=1:theMesh.numberOfBoundaryPatches
        boundaryPatchRef.value = propertyValue;
        boundaryPatchRef.type = 'zeroGradient';
        theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
    end
    
    cfdSetMeshField(theMeshField);
    
    Region.foamDictionary.transportProperties.(propertyName).name = propertyName;
    Region.foamDictionary.transportProperties.(propertyName).dimensions = propertyDimensions;
    Region.foamDictionary.transportProperties.(propertyName).propertyValue = propertyValue;
    
    % Update scale
    cfdUpdateScale(propertyName);
end

propertyNames = cell(length(properties),1);
for iProp=1:length(properties)
    propertyNames{iProp} = properties{iProp}.key;
end

% Setup density if not defined by user
if isempty(find(strcmp(propertyNames,'rho')))
    % Setup mesh field
    cfdSetupMeshField('rho', 'volScalarField');
    
    % Store initial value in mesh field
    theMeshField = cfdGetMeshField('rho');
    theMeshField.name = 'rho';
    theMeshField.dimensions = [0 0 0 0 0 0 0];
    theMeshField.phi(:) = 1.0;
    
    % Set zeroGradient bc type for all patches
    for iBPatch=1:theMesh.numberOfBoundaryPatches
        boundaryPatchRef.value = 1.0;
        boundaryPatchRef.type = 'zeroGradient';
        theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
    end
    
    cfdSetMeshField(theMeshField);
    
    % Update scale
    cfdUpdateScale('rho');
end

% Set mode
Region.compressible = false;

% Setup viscosity if not defined by user
if isempty(find(strcmp(propertyNames,'mu')))
    % Setup mesh field
    cfdSetupMeshField('mu', 'volScalarField');
    
    % Store initial value in mesh field
    theMeshField = cfdGetMeshField('mu');
    theMeshField.name = 'mu';
    theMeshField.dimensions = [0 0 0 0 0 0 0];
    theMeshField.phi(:) = 1.0e-3;
    
    % Set zeroGradient bc type for all patches
    for iBPatch=1:theMesh.numberOfBoundaryPatches
        boundaryPatchRef.value = 1.0e-3;
        boundaryPatchRef.type = 'zeroGradient';
        theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
    end
    
    cfdSetMeshField(theMeshField);
end

% Setup specific heat if not defined by user
if isempty(find(strcmp(propertyNames,'Cp')))
    % Setup mesh field
    cfdSetupMeshField('Cp', 'volScalarField');
    
    % Store initial value in mesh field
    theMeshField = cfdGetMeshField('Cp');
    theMeshField.name = 'Cp';
    theMeshField.dimensions = [0 0 0 0 0 0 0];
    theMeshField.phi(:) = 1004;
    
    % Set zeroGradient bc type for all patches
    for iBPatch=1:theMesh.numberOfBoundaryPatches
        boundaryPatchRef.type = 'zeroGradient';
        theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
    end
    
    cfdSetMeshField(theMeshField);
end

% Setup specific heat if not defined by user
if isempty(find(strcmp(propertyNames,'k')))
    if find(strcmp(propertyNames,'DT'))
        % Setup mesh field
        cfdSetupMeshField('k', 'volScalarField');
        
        % Store initial value in mesh field
        theMeshField = cfdGetMeshField('k');
        theMeshField.name = 'k';
        theMeshField.dimensions = [0 0 0 0 0 0 0];
        
        theSpecificHeatField = cfdGetMeshField('Cp');
        Cp = theSpecificHeatField.phi;
        
        theDiffusionCoefficientField = cfdGetMeshField('DT');
        Pr = theDiffusionCoefficientField.phi;
        
        theViscosityField = cfdGetMeshField('rho');
        mu = theViscosityField.phi;
        
        theMeshField.phi(:) = Pr.*mu.*Cp;
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:theMesh.numberOfBoundaryPatches
            boundaryPatchRef.value = 0.0;
            boundaryPatchRef.type = 'zeroGradient';
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theMeshField);
    elseif find(strcmp(propertyNames,'Pr'))
        % Setup mesh field
        cfdSetupMeshField('k', 'volScalarField');
        
        % Store initial value in mesh field
        theMeshField = cfdGetMeshField('k');
        theMeshField.name = 'k';
        theMeshField.dimensions = [0 0 0 0 0 0 0];
        
        theSpecificHeatField = cfdGetMeshField('Cp');
        Cp = theSpecificHeatField.phi;
        
        theDiffusionCoefficientField = cfdGetMeshField('Pr');
        Pr = theDiffusionCoefficientField.phi;
        
        theViscosityField = cfdGetMeshField('mu');
        mu = theViscosityField.phi;
        
        theMeshField.phi(:) = mu.*Cp./Pr;
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:theMesh.numberOfBoundaryPatches
            boundaryPatchRef.value = 0.0;
            boundaryPatchRef.type = 'zeroGradient';
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theMeshField);        
    end
end