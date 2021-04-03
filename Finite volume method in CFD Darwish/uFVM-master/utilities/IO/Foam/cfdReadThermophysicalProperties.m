function cfdReadThermophysicalProperties
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads thermo-physical properties file
%--------------------------------------------------------------------------
global Region;

caseDirectoryPath = cfdGetCaseDirectoryPath;

thermophysicalPropertiesFile = [caseDirectoryPath, filesep, 'constant', filesep, 'thermophysicalProperties'];

% Check if "transportProperties" exists
if exist(thermophysicalPropertiesFile, 'file')~=2
    return;
end

fprintf('\nReading Thermophysical Properties ...');

% Open thermophysicalProperties file in read mode
fid = fopen(thermophysicalPropertiesFile, 'r');

% Read thermoType block
thermoTypeBlock = cfdReadCfdDictionary(fid, 'thermoType');

% Store
entryNames = fieldnames(thermoTypeBlock);
for iEntry=1:length(fieldnames(thermoTypeBlock))
    Region.foamDictionary.thermophysicalProperties.thermoType.(entryNames{iEntry}) = thermoTypeBlock.(entryNames{iEntry});
end


if strcmp(thermoTypeBlock.mixture, 'pureMixture')
    
    % specie
    specieBlock = cfdReadSubCfdDictionary(fid, 'mixture', 'specie');
    
    entryNames = fieldnames(specieBlock);
    for iEntry=1:length(fieldnames(specieBlock))
        Region.foamDictionary.thermophysicalProperties.mixture.specie.(entryNames{iEntry}) = eval(specieBlock.(entryNames{iEntry}));
    end
    
    % Thermodynamics
    thermodynamicsBlock = cfdReadSubCfdDictionary(fid, 'mixture', 'thermodynamics');
    
    entryNames = fieldnames(thermodynamicsBlock);
    for iEntry=1:length(fieldnames(thermodynamicsBlock))
        Region.foamDictionary.thermophysicalProperties.mixture.thermodynamics.(entryNames{iEntry}) = eval(thermodynamicsBlock.(entryNames{iEntry}));
    end
    
    % transport
    transportBlock = cfdReadSubCfdDictionary(fid, 'mixture', 'transport');
    
    entryNames = fieldnames(transportBlock);
    for iEntry=1:length(fieldnames(transportBlock))
        Region.foamDictionary.thermophysicalProperties.mixture.transport.(entryNames{iEntry}) = eval(transportBlock.(entryNames{iEntry}));
    end
    
    
    % Store transport model
    if strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.transport, 'const')        
        
        % Setup mesh field
        cfdSetupMeshField('mu', 'volScalarField');
        
        % Store initial value in mesh field
        theMeshField = cfdGetMeshField('mu');
        theMeshField.name = 'mu';
        theMeshField.dimensions = [0,0,0,0,0,0,0];
        theMeshField.phi(:) = Region.foamDictionary.thermophysicalProperties.mixture.transport.mu;
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theMeshField);
        
        
        %
        % Prandtl number
        %
        
        % Setup mesh field
        cfdSetupMeshField('Pr', 'volScalarField');
        
        % Store initial value in mesh field
        theMeshField = cfdGetMeshField('Pr');
        theMeshField.name = 'Pr';
        theMeshField.dimensions = [0,0,0,0,0,0,0];
        theMeshField.phi(:) = Region.foamDictionary.thermophysicalProperties.mixture.transport.Pr;
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theMeshField);

    elseif strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.transport, 'sutherland')
        
        if ~cfdIsFieldAvailable('T')
            error('\nT is not found for sutherland model\n');
        end
        
        As = Region.foamDictionary.thermophysicalProperties.mixture.transport.As;
        Ts = Region.foamDictionary.thermophysicalProperties.mixture.transport.Ts;
        
        % Setup mesh field
        cfdSetupMeshField('mu', 'volScalarField');
        
        % Store initial value in mesh field
        theMeshField = cfdGetMeshField('mu');
        theMeshField.name = 'mu';
        theMeshField.dimensions = [0,0,0,0,0,0,0];
        
        T = cfdGetDataArray('T');
        
        theMeshField.phi(:) = As*sqrt(T)./(1 + Ts./T);
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theMeshField);        
        
    elseif strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.transport, 'polynomial')
        error('\n%s\n','polynomial transport model is not implemented');
    end
    
    % Store thermodynamics model
    if strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.thermo, 'hConst')
        
        %
        % Prandtl number
        %        
        
        % Setup mesh field
        cfdSetupMeshField('Cp', 'volScalarField');
        
        % Store initial value in mesh field
        theSpecificHeatMeshField = cfdGetMeshField('Cp');
        theSpecificHeatMeshField.name = 'Cp';
        theSpecificHeatMeshField.dimensions = [0,0,0,0,0,0,0];
        theSpecificHeatMeshField.phi(:) = Region.foamDictionary.thermophysicalProperties.mixture.thermodynamics.Cp;
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theSpecificHeatMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theSpecificHeatMeshField);
        
        %
        % Thermal conductivity
        %
        
        % Setup mesh field
        cfdSetupMeshField('k', 'volScalarField');
        
        % Store initial value in mesh field
        theConductivityMeshField = cfdGetMeshField('k');
        theConductivityMeshField.name = 'k';
        theConductivityMeshField.dimensions = [0,0,0,0,0,0,0];
        
        Pr = cfdGetDataArray('Pr');
        mu = cfdGetDataArray('mu');
        Cp = cfdGetDataArray('Cp');
        
        theConductivityMeshField.phi(:) = Cp.*mu./Pr;
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theConductivityMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theConductivityMeshField);
        
    elseif strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.thermo, 'hPolynomial')
        error('\n%s\n','hPolynomial thermo model is not implemented');
    end
    
    % Store equationOfState model
    if strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.equationOfState, 'perfectGas')
        
        Region.compressible = true;
        
        % Setup mesh field
        cfdSetupMeshField('rho', 'volScalarField');
        
        % Store initial value in mesh field
        theRhoMeshField = cfdGetMeshField('rho');
        theRhoMeshField.name = 'rho';
        theRhoMeshField.dimensions = [0,0,0,0,0,0,0];
        
        p = cfdGetDataArray('p');
        T = cfdGetDataArray('T');
        
        % Get molecular weight
        molWeight = Region.foamDictionary.thermophysicalProperties.mixture.specie.molWeight;
        Rbar = 8.314e3; % universal gas constant
        R = Rbar / molWeight;        
        
        theRhoMeshField.phi(:) = p./(R*T);
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theRhoMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theRhoMeshField);
        
        % Update scale
        cfdUpdateScale('rho');
        
        %
        % 1/(RT) field
        %
        % Setup mesh field
        cfdSetupMeshField('drhodp', 'volScalarField');
        
        % Store initial value in mesh field
        theMeshField = cfdGetMeshField('drhodp');
        theMeshField.name = 'drhodp';
        theMeshField.dimensions = [0,0,0,0,0,0,0];
        
        theMeshField.phi(:) = 1./(R*T);
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theMeshField);
        
    elseif strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.equationOfState, 'Boussinesq')
        Region.compressible = false;
        
        % Setup mesh field
        cfdSetupMeshField('rho', 'volScalarField');
        
        % Store initial value in mesh field
        theRhoMeshField = cfdGetMeshField('rho');
        theRhoMeshField.name = 'rho';
        theRhoMeshField.dimensions = [0,0,0,0,0,0,0];
        
        T = cfdGetDataArray('T');
        
        beta = Region.foamDictionary.thermophysicalProperties.mixture.thermodynamics.beta;
        TRef = Region.foamDictionary.thermophysicalProperties.mixture.thermodynamics.TRef;
        rhoRef = Region.foamDictionary.thermophysicalProperties.mixture.thermodynamics.rhoRef;
        
        theRhoMeshField.phi(:) = rhoRef*(1-beta*(T-TRef));
        
        % Set zeroGradient bc type for all patches
        for iBPatch=1:cfdGetNumberOfBPatches
            boundaryPatchRef.type = 'zeroGradient';
            theRhoMeshField.boundaryPatchRef{iBPatch} = boundaryPatchRef;
        end
        
        cfdSetMeshField(theRhoMeshField);
        
        % Update scale
        cfdUpdateScale('rho');        
        
    elseif strcmp(Region.foamDictionary.thermophysicalProperties.thermoType.equationOfState, 'incompressiblePerfectGas')
        error('\n%s\n','incompressiblePerfectGas equationOfState model is not implemented');        
    end   
end

