function cfdSetupEquation(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Set momentum equations terms
%--------------------------------------------------------------------------

% Equation name
theEquation.name = theEquationName;

% Residuals
if strcmp(theEquationName,'U')
    theEquation.residuals.rmsResidual = [1,1,1];
    theEquation.residuals.maxResidual = [1,1,1];
    theEquation.residuals.initResidual = [1,1,1];
    theEquation.residuals.finalResidual = [1,1,1];
else
    theEquation.residuals.rmsResidual = 1;
    theEquation.residuals.maxResidual = 1;
    theEquation.residuals.initResidual = 1;
    theEquation.residuals.finalResidual = 1;    
end

% Store
cfdSetModel(theEquation);