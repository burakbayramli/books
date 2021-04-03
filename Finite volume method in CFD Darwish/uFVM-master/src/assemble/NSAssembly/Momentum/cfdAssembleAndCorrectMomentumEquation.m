function cfdAssembleAndCorrectMomentumEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles and corrects the momentum equation
%--------------------------------------------------------------------------

if ~cfdIsSolveEquation('U')
    return;
end

for iComponent=1:3
    theCoefficients = cfdSetupCoefficients;
    cfdSetCoefficients(theCoefficients);
    
    % Assemble Equation Terms
    cfdAssembleMomentumEquation(iComponent);
    
    % Solve momentum equation
    cfdSolveMomentumEquation(iComponent);
    
    % Reset coefficients
    cfdZeroCoefficients;
end
