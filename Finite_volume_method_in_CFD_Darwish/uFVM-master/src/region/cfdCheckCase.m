function cfdCheckCase
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if the case setup is ok
%--------------------------------------------------------------------------

% Check if all equations are consistent in time
theEquationNames = cfdGetEquationNames;
theNumberOfEquations = length(theEquationNames);

if cfdIsTransient  
    for iEquation=1:theNumberOfEquations
        theEquationName = theEquationNames{iEquation};
        if ~strcmp(theEquationName, 'p')
            transientTerm = cfdGetTermInEquation(theEquationName, 'Transient');
            if isempty(transientTerm)
                error('\n%s','The equations are not consistent in time');
            end
        end
    end
else
    for iEquation=1:theNumberOfEquations
        theEquationName = theEquationNames{iEquation};
        if ~strcmp(theEquationName, 'p')
            transientTerm = cfdGetTermInEquation(theEquationName, 'Transient');
            if ~isempty(transientTerm)
                warning('\n%s',[theEquationName, ' equation includes a transient term and will be ignored']);
            end
        end
    end        
end 