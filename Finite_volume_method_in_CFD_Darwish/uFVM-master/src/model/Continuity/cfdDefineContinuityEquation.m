function cfdDefineContinuityEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%   Setup continuity equation.
%--------------------------------------------------------------------------

% Check if a file of the same name of the equation name exists
if exist(['0', filesep, 'p'], 'file')~=2
    list = dir;
    folderName = list(1).folder;
    [pathstr,name,ext] = fileparts(folderName);
    error('\n%s\n', ['A file of the name ', 'p', ' doesn''t exist in ', name,'/0 directory.']);
end

% Create theEquation structure
cfdSetupEquation('p');

%  Add terms
cfdSetContinuityTerms({'massDivergenceTerm'});

if cfdIsCompressible
    if cfdIsTransient
        cfdAddTerm('p', 'Transient');
    else
        cfdAddTerm('p', 'FalseTransient');
    end
end

% Initialize mdot_f from U if not already available in data base
if ~cfdIsFieldAvailable('mdot_f')
    if cfdIsFieldAvailable('U')
        cfdSetupMeshField('mdot_f', 'surfaceScalarField', 'dimensions', [0,0,0,0,0,0,0]);  
        cfdInitializeMdotFromU;
    end  
end

% Setup DU field
cfdSetupMeshField('DU1', 'volScalarField');
cfdSetupMeshField('DU2', 'volScalarField');
cfdSetupMeshField('DU3', 'volScalarField');

cfdSetConstantBC('DU1', 'zeroGradient');
cfdSetConstantBC('DU2', 'zeroGradient');
cfdSetConstantBC('DU3', 'zeroGradient');

% Setup DU field
cfdSetupMeshField('DUT1', 'volScalarField');
cfdSetupMeshField('DUT2', 'volScalarField');
cfdSetupMeshField('DUT3', 'volScalarField');

cfdSetConstantBC('DUT1', 'zeroGradient');
cfdSetConstantBC('DUT2', 'zeroGradient');
cfdSetConstantBC('DUT3', 'zeroGradient');

% Setup Pressure Correction field and assign it the cfdBoundary conditions of
% pressure field
cfdSetupMeshField('pp', 'volScalarField'); 
thePressureField = cfdGetMeshField('p');
thePPField = cfdGetMeshField('pp');
thePPField.boundaryPatchRef = thePressureField.boundaryPatchRef;

theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    if strcmp(thePPField.boundaryPatchRef{iBPatch}.type, 'fixedValue')
        thePPField.boundaryPatchRef{iBPatch}.value = 0;
    end
end
cfdSetMeshField(thePPField);
