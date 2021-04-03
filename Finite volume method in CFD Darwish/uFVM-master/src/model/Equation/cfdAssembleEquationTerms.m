function cfdAssembleEquationTerms(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles equation terms
%--------------------------------------------------------------------------

% get theScalarName
theEquation = cfdGetModel(theEquationName);

% check if equation is to be assembled
theNumberOfTerms = length(theEquation.terms);

% Assemble Coefficients
for iTerm = 1:theNumberOfTerms
    theTermName = theEquation.terms{iTerm};
    if strcmp(theTermName,'Transient')
        cfdZeroElementFLUXCoefficients;
        cfdAssembleTransientTerm(theEquationName);
        cfdAssembleIntoGlobalMatrixElementFluxes;
    elseif strcmp(theTermName, 'Convection')
        cfdZeroFaceFLUXCoefficients;
        cfdAssembleConvectionTerm(theEquationName);
        cfdAssembleDCSchemeTerm(theEquationName);
        cfdAssembleIntoGlobalMatrixFaceFluxes;
        
        cfdZeroElementFLUXCoefficients;
        cfdAssembleDivergenceCorrectionTerm(theEquationName);
        cfdAssembleIntoGlobalMatrixElementFluxes;
    elseif strcmp(theTermName, 'Diffusion')
        cfdZeroFaceFLUXCoefficients;
        cfdAssembleDiffusionTerm(theEquationName); 
        cfdAssembleIntoGlobalMatrixFaceFluxes;
    elseif strcmp(theTermName,'FalseTransient')
        cfdZeroElementFLUXCoefficients;
        cfdAssembleFalseTransientTerm(theEquationName);
        cfdAssembleIntoGlobalMatrixElementFluxes;        
    else
        error('\n%s\n',[theTermName,' term is not defined']);
    end
end
