function cfdCorrectEquation(theEquationName,iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function corrects the equations
%--------------------------------------------------------------------------

if strcmp(theEquationName, 'p')
    % Correct PP field
    cfdCorrectPPField;
    
    % Correct U
    cfdCorrectVelocityField;
    
    % Correct p
    cfdCorrectPressureEquation;
    
    % Correct rho, if the flow is compressible
    if cfdIsCompressible
        cfdCorrectDensityField;
    end
    
    % Correct mdot_f
    cfdCorrectMdotField;
else
    cfdCorrectScalarEquation(theEquationName, iComponent);
end
