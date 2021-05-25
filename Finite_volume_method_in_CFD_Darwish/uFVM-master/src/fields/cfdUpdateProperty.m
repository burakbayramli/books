function cfdUpdateProperty(thePropertyName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates the property according to its model
%--------------------------------------------------------------------------

theFoamDict = cfdGetFoamDict;

if isfield(theFoamDict, 'thermophysicalProperties')
    thermophysicalPropertiesDict = cfdGetThermophysicalPropertiesDict;
else
    return;
end

if strcmp(thePropertyName, 'rho')
    if strcmp(thermophysicalPropertiesDict.thermoType.equationOfState, 'prefectGas')
        theDensityField = cfdGetMeshField('rho');
        
        p = cfdGetDataArray('p');
        T = cfdGetDataArray('T');
        
        molWeight = thermophysicalPropertiesDict.mixture.specie.molWeight;
        Rbar = 8.314e3; % universal gas constant
        R = Rbar / molWeight;
        
        theDensityField.phi(:) = p./(R*T);
        cfdSetMeshField(theDensityField);
    elseif strcmp(thermophysicalPropertiesDict.thermoType.equationOfState, 'Boussinesq')
        theDensityField = cfdGetMeshField('rho');
        
        beta = thermophysicalPropertiesDict.mixture.thermodynamics.beta;
        TRef = thermophysicalPropertiesDict.mixture.thermodynamics.TRef;
        rhoRef = thermophysicalPropertiesDict.mixture.thermodynamics.rhoRef;
        
        T = cfdGetDataArray('T');
        
        theDensityField.phi(:) = rhoRef*(1-beta*(T-TRef));
        cfdSetMeshField(theDensityField);
    end
elseif strcmp(thePropertyName, 'drhodp')
    if strcmp(thermophysicalPropertiesDict.thermoType.equationOfState, 'prefectGas')
        theDrhodpField = cfdGetMeshField('drhodp');
        T = cfdGetDataArray('T');
        
        molWeight = thermophysicalPropertiesDict.mixture.specie.molWeight;
        Rbar = 8.314e3; % universal gas constant
        R = Rbar / molWeight;
        
        theDrhodpField.phi(:) = 1./(R*T);
        cfdSetMeshField(theDrhodpField);
    end
elseif strcmp(thePropertyName, 'mu')
    if strcmp(thermophysicalPropertiesDict.thermoType.transport, 'sutherland')
        
        theViscosityField = cfdGetMeshField('mu');
        As = thermophysicalPropertiesDict.mixture.transport.As;
        Ts = thermophysicalPropertiesDict.mixture.transport.Ts;
        
        T = cfdGetDataArray('T');
        
        theViscosityField.phi(:) = As*sqrt(T)./(1 + Ts./T);
    end
end

% For boundary patches
cfdUpdateScalarFieldForAllBoundaryPatches(thePropertyName);




