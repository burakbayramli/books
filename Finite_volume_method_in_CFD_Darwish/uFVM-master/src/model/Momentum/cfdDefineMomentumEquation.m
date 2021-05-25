function cfdDefineMomentumEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Setup momentum equation
%--------------------------------------------------------------------------

% Check if a file of the same name of the equation name exists
if exist(['0', filesep, 'U'], 'file')~=2
    list = dir;
    folderName = list(1).folder;
    [pathstr,name,ext] = fileparts(folderName);
    error('\n%s\n', ['A file of the name ', 'U', ' doesn''t exist in ', name,'/0 directory.']);
end

% Create theEquation structure
cfdSetupEquation('U');

%  Add terms
cfdSetMomentumTerms({'Convection', 'Stress', 'PressureGradient'});

if cfdIsTransient
    cfdAddTerm('U', 'Transient');
else
    cfdAddTerm('U', 'FalseTransient');
end

if cfdIsWithBuoyancy
    cfdAddTerm('U', 'Buoyancy');
end
