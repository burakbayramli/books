function cfdInitMomentumEquationResiduals
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    
%--------------------------------------------------------------------------

theEquation = cfdGetModel('U');

theEquation.residuals.rmsResidual = [1, 1, 1];
theEquation.residuals.maxResidual = [1, 1, 1];
theEquation.residuals.initResidual = [1, 1, 1];
theEquation.residuals.finalResidual = [1, 1, 1];


% Store
cfdSetModel(theEquation);
