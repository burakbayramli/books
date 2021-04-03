function cfdInitEquationResiduals(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    
%--------------------------------------------------------------------------

theEquation = cfdGetModel(theEquationName);

theEquation.residuals.rmsResidual = 1;
theEquation.residuals.maxResidual = 1;
theEquation.residuals.initResidual = 1;
theEquation.residuals.finalResidual = 1;

% Store
cfdSetModel(theEquation);
