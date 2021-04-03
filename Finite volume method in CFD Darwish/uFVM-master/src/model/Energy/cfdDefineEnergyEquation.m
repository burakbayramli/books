function cfdDefineEnergyEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Setup energy equation
%--------------------------------------------------------------------------

% Check if a file of the same name of the equation name exists
if exist([cfdGetCaseDirectoryPath, filesep, '0', filesep, 'T'], 'file')~=2
    return;
end

% Create structure
cfdSetupEquation('T');

cfdSetEnergyTerms({'Convection', 'Diffusion'});

if cfdIsTransient
    cfdAddTerm('T', 'Transient');
else
    cfdAddTerm('T', 'FalseTransient');
end

