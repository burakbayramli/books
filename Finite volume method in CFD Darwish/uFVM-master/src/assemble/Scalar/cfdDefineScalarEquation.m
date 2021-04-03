function cfdDefineScalarEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Setup scalar equation
%--------------------------------------------------------------------------

% Check if a file of the same name of the equation name exists
if exist([cfdGetCaseDirectoryPath, filesep, '0', filesep, 'T'], 'file')~=2
    return;
end

% Print Equation
fprintf('\n-------------------------------------------------');

% Create structure
cfdSetupEquation('T');

cfdSetEnergyTerms({'Convection', 'Diffusion'});

if cfdIsTransient
    cfdAddTerm('T', 'Transient');
else
    cfdAddTerm('T', 'falseTransientTerm');
end

cfdInitEnergyEquationResiduals;

