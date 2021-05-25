function cfdSetEquationResiduals(theEquationName, rmsResidual, maxResidual, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function stores equation residuals
%--------------------------------------------------------------------------

if nargin==3
    iComponent = 1;
end

global Region;

Region.model.(theEquationName).residuals.rmsResidual(iComponent) = rmsResidual;
Region.model.(theEquationName).residuals.maxResidual(iComponent) = maxResidual;