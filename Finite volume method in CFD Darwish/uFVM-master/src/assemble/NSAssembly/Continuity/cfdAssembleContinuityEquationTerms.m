function cfdAssembleContinuityEquationTerms
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles equation terms
%--------------------------------------------------------------------------

% get theScalarName
theEquation = cfdGetModel('p');

% check if equation is to be assembled
theNumberOfTerms = length(theEquation.terms);

% Assemble Coefficients
for iTerm = 1:theNumberOfTerms
    theTermName = theEquation.terms{iTerm};
    if strcmp(theTermName,'massDivergenceTerm')
        cfdZeroFaceFLUXCoefficients;
        cfdAssembleMassDivergenceTerm; 
        
        if cfdIsCompressible
            cfdAssembleMassDivergenceAdvectionTerm;
        end        

		cfdStoreMassFlowRate;
        
        % Assmeble into global Matrix
        cfdAssembleIntoGlobalMatrixFaceFluxes;
    elseif strcmp(theTermName,'Transient')
        
    elseif strcmp(theTermName,'FalseTransient')
        
    else
        error('\n%s\n',[theTermName,' term is not defined']);
    end
end
