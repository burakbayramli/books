function cfdUpdatePrevIter
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
    theVelocityField.prevIter.phi = theVelocityField.phi;
    cfdSetMeshField(theVelocityField);
end

if cfdIsFieldAvailable('p')
    thePressureField = cfdGetMeshField('p');
    thePressureField.prevIter.phi = thePressureField.phi;  
    cfdSetMeshField(thePressureField);
end

if cfdIsFieldAvailable('T')
    theTemperatureField = cfdGetMeshField('T');
    theTemperatureField.prevIter.phi = theTemperatureField.phi;  
    cfdSetMeshField(theTemperatureField);
end

if cfdIsFieldAvailable('rho')
    theRhoField = cfdGetMeshField('rho');
    theRhoField.prevIter.phi = theRhoField.phi;
    cfdSetMeshField(theRhoField);
end

if cfdIsFieldAvailable('mdot_f')
    theMdotField = cfdGetMeshField('mdot_f');
    theMdotField.prevIter.phi = theMdotField.phi;  
    cfdSetMeshField(theMdotField);
end

if cfdIsFieldAvailable('Cp')
    theSpecificHeatField = cfdGetMeshField('Cp');
    theSpecificHeatField.prevIter.phi = theSpecificHeatField.phi;  
    cfdSetMeshField(theSpecificHeatField);
end

if cfdIsFieldAvailable('mu')
    theViscosityField = cfdGetMeshField('mu');
    theViscosityField.prevIter.phi = theViscosityField.phi;  
    cfdSetMeshField(theViscosityField);
end
