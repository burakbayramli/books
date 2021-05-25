function cfdAssembleAndCorrectContinuityEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles and corrects the continuity equation
%--------------------------------------------------------------------------

if ~cfdIsSolveEquation('p')
    return;
end

theCoefficients = cfdSetupCoefficients;
cfdSetCoefficients(theCoefficients);

% Assemble Equation
cfdAssembleContinuityEquation;

% Solve
cfdSolveContinuityEquation;

% Reset coefficients to zero
cfdZeroCoefficients;

