function cfdAssembleMomentumEquationTerms(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles equation terms
%--------------------------------------------------------------------------

% get theScalarName
theEquation = cfdGetModel('U');

% check if equation is to be assembled
theNumberOfTerms = length(theEquation.terms);

% Assemble Coefficients
for iTerm = 1:theNumberOfTerms
    theTermName = theEquation.terms{iTerm};
    if strcmp(theTermName,'Transient')
        cfdZeroElementFLUXCoefficients;
        cfdAssembleMomentumTransientTerm(iComponent);
        cfdAssembleIntoGlobalMatrixElementFluxes;
    elseif strcmp(theTermName, 'Convection')
        cfdZeroFaceFLUXCoefficients;
        cfdAssembleMomentumConvectionTerm(iComponent);
        cfdAssembleMomentumDCSchemeTerm(iComponent);
        cfdAssembleIntoGlobalMatrixFaceFluxes;
        
        cfdZeroElementFLUXCoefficients;
        cfdAssembleMomentumDivergenceCorrectionTerm(iComponent);
        cfdAssembleIntoGlobalMatrixElementFluxes;        
    elseif strcmp(theTermName, 'Stress')
        cfdZeroFaceFLUXCoefficients;
        cfdAssembleStressTerm(iComponent);
        cfdAssembleIntoGlobalMatrixFaceFluxes;
    elseif strcmp(theTermName,'PressureGradient')
        cfdZeroElementFLUXCoefficients;
        cfdAssemblePressureGradientTerm(iComponent); 
        cfdAssembleIntoGlobalMatrixElementFluxes;
    elseif strcmp(theTermName,'FalseTransient')
        cfdZeroElementFLUXCoefficients;
        cfdAssembleMomentumFalseTransientTerm(iComponent);
        cfdAssembleIntoGlobalMatrixElementFluxes; 
    elseif strcmp(theTermName,'Buoyancy')
        cfdZeroElementFLUXCoefficients;
        cfdAssembleMomentumBuoyancyTerm(iComponent);
        cfdAssembleIntoGlobalMatrixElementFluxes;         
    else
        error('\n%s\n',[theTermName,' term is not defined']);        
    end    
end
