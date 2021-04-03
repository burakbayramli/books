function cfdRunFASCase
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function cfdRuns the problem by iterating until convergence the
%   different equations previously defined by the user
%--------------------------------------------------------------------------

if cfdIsTransient
    
else
    cfdRunFalseTransientFASCase;
end
