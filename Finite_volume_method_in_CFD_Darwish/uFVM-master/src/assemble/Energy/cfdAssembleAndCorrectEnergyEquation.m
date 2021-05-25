function cfdAssembleAndCorrectEnergyEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles and corrects the energy equation
%--------------------------------------------------------------------------

if ~cfdIsSolveEquation('T')
    return;
end

theCoefficients = cfdSetupCoefficients;
cfdSetCoefficients(theCoefficients);

% Assemble Equation
cfdAssembleEnergyEquation;

% Solve
cfdSolveEnergyEquation;

% Reset coefficients to zero
cfdZeroCoefficients;

