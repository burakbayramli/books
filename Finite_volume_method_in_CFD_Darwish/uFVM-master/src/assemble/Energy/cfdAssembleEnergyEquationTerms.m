function cfdAssembleEnergyEquationTerms
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles energy equation terms
%--------------------------------------------------------------------------

% get theScalarName
theEquation = cfdGetModel('T');

% check if equation is to be assembled
theNumberOfTerms = length(theEquation.terms);

% Assemble Coefficients
for iTerm = 1:theNumberOfTerms
    theTermName = theEquation.terms{iTerm};
    if strcmp(theTermName,'Transient')
        cfdZeroElementFLUXCoefficients;
        cfdAssembleEnergyTransientTerm;
        cfdAssembleIntoGlobalMatrixElementFluxes;
    elseif strcmp(theTermName, 'Convection')
        cfdZeroFaceFLUXCoefficients;
        cfdAssembleEnergyConvectionTerm;
        cfdAssembleEnergyDCSchemeTerm;
        cfdAssembleIntoGlobalMatrixFaceFluxes;
        
        cfdZeroElementFLUXCoefficients;
        cfdAssembleEnergyDivergenceCorrectionTerm;
        cfdAssembleIntoGlobalMatrixElementFluxes;
    elseif strcmp(theTermName, 'Diffusion')
        cfdZeroFaceFLUXCoefficients;
        cfdAssembleEnergyDiffusionTerm; 
        cfdAssembleIntoGlobalMatrixFaceFluxes;
    elseif strcmp(theTermName,'FalseTransient')
        cfdZeroElementFLUXCoefficients;
        cfdAssembleEnergyFalseTransientTerm;
        cfdAssembleIntoGlobalMatrixElementFluxes;        
    else
        error('\n%s\n',[theTermName,' term is not defined']);
    end
end
