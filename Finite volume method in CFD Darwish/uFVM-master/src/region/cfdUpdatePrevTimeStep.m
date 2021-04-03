function cfdUpdatePrevTimeStep
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function updates previous time step fields
%--------------------------------------------------------------------------

if cfdIsFieldAvailable('U')
    theVelocityField = cfdGetMeshField('U');
    theVelocityField.prevTimeStep.phi = theVelocityField.phi;
    cfdSetMeshField(theVelocityField);
end

if cfdIsFieldAvailable('p')
    thePressureField = cfdGetMeshField('p');
    thePressureField.prevTimeStep.phi = thePressureField.phi;  
    cfdSetMeshField(thePressureField);
end

if cfdIsFieldAvailable('T')
    theTemperatureField = cfdGetMeshField('T');
    theTemperatureField.prevTimeStep.phi = theTemperatureField.phi;  
    cfdSetMeshField(theTemperatureField);
end

if cfdIsFieldAvailable('rho')
    theRhoField = cfdGetMeshField('rho');
    theRhoField.prevTimeStep.phi = theRhoField.phi;
    cfdSetMeshField(theRhoField);
end

if cfdIsFieldAvailable('mdot_f')
    theMdotField = cfdGetMeshField('mdot_f');
    theMdotField.prevTimeStep.phi = theMdotField.phi;  
    cfdSetMeshField(theMdotField);
end

if cfdIsFieldAvailable('Cp')
    theSpecificHeatField = cfdGetMeshField('Cp');
    theSpecificHeatField.prevTimeStep.phi = theSpecificHeatField.phi;  
    cfdSetMeshField(theSpecificHeatField);
end

if cfdIsFieldAvailable('mu')
    theViscosityField = cfdGetMeshField('mu');
    theViscosityField.prevTimeStep.phi = theViscosityField.phi;  
    cfdSetMeshField(theViscosityField);
end

