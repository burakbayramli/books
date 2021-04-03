function cfdPostAssembleMomentumEquation(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

% Apply under-relaxation
cfdAssembleImplicitRelaxation('U');

% Store DU and DUT
cfdAssembleDCoefficients(iComponent);

% Compute RMS and MAX Residuals
cfdComputeScaledRMSResiduals('U', iComponent);

