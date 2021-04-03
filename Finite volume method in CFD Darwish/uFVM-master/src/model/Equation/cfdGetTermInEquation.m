function theTerm = cfdGetTermInEquation(theEquationName,theTermName,varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function gets a term from the equation
%--------------------------------------------------------------------------

theEquation = cfdGetModel(theEquationName);
theNumberOfTerms = length(theEquation.terms);
theTerm = '';

for iTerm=1:theNumberOfTerms    
    if strcmp(theTermName,theEquation.terms{iTerm}.name)
        if nargin==2
            theTerm = theEquation.terms{iTerm};
        else
            theTermExpression = varargin{1}; 
            if strcmp(theTermExpression, theEquation.terms{iTerm}.formula)
                theTerm = theEquation.terms{iTerm};
            end
        end
    end
end