function cfdSmooth(iLevel, nIters)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------
for iter=1:nIters
    
    % Momentum
    cfdAssembleAndCorrectMomentumEquation;
    
    % Continuity
    cfdAssembleAndCorrectContinuityEquation;
    
end
