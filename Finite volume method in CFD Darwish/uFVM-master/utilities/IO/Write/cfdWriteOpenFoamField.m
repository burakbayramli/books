function cfdWriteOpenFoamField(theMeshFieldName, timeStep)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function writes the field in OpenFOAM format
%--------------------------------------------------------------------------

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
theNumberOfElements = cfdGetNumberOfElements;

% Get mesh field
theMeshField = cfdGetMeshField(theMeshFieldName);

% Concatenate header
text = ...
    {'/*--------------------------------*- C++ -*----------------------------------*\\', ...
    '| =========                 |                                                 |', ...
    '| \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |', ...
    '|  \\    /   O peration     | Version:  v1706                                 |', ...
    '|   \\  /    A nd           | Web:      www.OpenFOAM.com                      |', ...
    '|    \\/     M anipulation  |                                                 |', ...
    '\\*---------------------------------------------------------------------------*/', ...
    'FoamFile', ...
    '{', ...
    '    version     2.0;', ...
    '    format      ascii;', ...
    ['    class       ', theMeshField.type,';'], ...
    ['    location    "', num2str(timeStep),'";'], ...
    ['    object      ', theMeshFieldName,';'], ...
    '}', ...
    '// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //', ...
    ''};

% Concatenate dimensions
text{end+1} = ['dimensions      ', theMeshField.dimensions];
text{end+1} = '';
text{end+1} = '';

% Concatenate internal field
if strcmp(theMeshField.type, 'volScalarField')
    text{end+1} = 'internalField   nonuniform List<scalar>';
    text{end+1} = num2str(theNumberOfElements);
    text{end+1} = '(';
    for iElement=1:theNumberOfElements
        text{end+1} = num2str(theMeshField.phi(iElement));
    end
    text{end+1} = ')';
    text{end+1} = ';';
elseif strcmp(theMeshField.type, 'volVectorField')
    text{end+1} = 'internalField   nonuniform List<vector>';
    text{end+1} = num2str(theNumberOfElements);
    text{end+1} = '(';
    for iElement=1:theNumberOfElements
        text{end+1} = ['(', strjoin({num2str(theMeshField.phi(iElement, 1)), num2str(theMeshField.phi(iElement, 2)), num2str(theMeshField.phi(iElement, 3))}), ')'];
    end
    text{end+1} = ')';
    text{end+1} = ';';
elseif strcmp(theMeshField.type, 'surfaceScalarField')
    text{end+1} = 'internalField   nonuniform List<scalar>';
    text{end+1} = num2str(theNumberOfInteriorFaces);
    text{end+1} = '(';
    for iFace=1:theNumberOfInteriorFaces
        text{end+1} = num2str(theMeshField.phi(iFace));
    end
    text{end+1} = ')';
    text{end+1} = ';';
end


% Concatenate boundary field
text{end+1} = '';
text{end+1} = 'boundaryField';
text{end+1} = '{';

theNumberOfPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfPatches
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    theNumberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);  
    iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
    iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);
    
    text{end+1} = ['    ', theBoundary.name];
    text{end+1} = '    {';
       
    if strcmp(theMeshField.type, 'volScalarField')

        type = theMeshField.boundaryPatchRef{iBPatch}.type;
        
        text{end+1} = ['        type             ', type, ';'];        
        text{end+1} = '        value            nonuniform List<scalar>';
        text{end+1} = num2str(theNumberOfBFaces);
        text{end+1} = '(';
        for iBElement=iBElements'
            text{end+1} = num2str(theMeshField.phi(iBElement));
        end
        text{end+1} = ')';
        text{end+1} = ';';
    elseif strcmp(theMeshField.type, 'surfaceScalarField')
        type = 'calculated';
        
        text{end+1} = ['        type             ', type, ';'];        
        text{end+1} = '        value            nonuniform List<scalar>';
        text{end+1} = num2str(theNumberOfBFaces);
        text{end+1} = '(';
        for iBFace=iBFaces'
            text{end+1} = num2str(theMeshField.phi(iBFace));
        end
        text{end+1} = ')';
        text{end+1} = ';';
    elseif strcmp(theMeshField.type, 'volVectorField')
        type = theMeshField.boundaryPatchRef{iBPatch}.type;
        
        text{end+1} = ['        type             ', type, ';'];        
        
        text{end+1} = '        value            nonuniform List<vector>';
        text{end+1} = num2str(theNumberOfBFaces);
        text{end+1} = '(';
        for iBElement=iBElements'
            text{end+1} = ['(', strjoin({num2str(theMeshField.phi(iBElement, 1)), num2str(theMeshField.phi(iBElement, 2)), num2str(theMeshField.phi(iBElement, 3))}), ')'];
        end
        text{end+1} = ')';
        text{end+1} = ';';
    end
    
    text{end+1} = '    }';
    
end
text{end+1} = '}';
text{end+1} = '';
text{end+1} = '// ************************************************************************* //';

% Print to time directory
fileID = fopen([num2str(timeStep), filesep, theMeshFieldName], 'w');
for i=1:length(text)
    fprintf(fileID, '%s\n', text{i});
end
fclose(fileID);
