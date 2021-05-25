function cfdPostResiduals
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function prints residuals to console
%--------------------------------------------------------------------------

if cfdIsSolveEquation('U')
    cfdPrintResiduals('U');
end

if cfdIsSolveEquation('p')
    cfdPrintResiduals('p');
end

if cfdIsSolveEquation('T')
    cfdPrintResiduals('T');
end