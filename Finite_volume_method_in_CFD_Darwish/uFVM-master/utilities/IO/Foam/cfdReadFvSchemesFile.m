function cfdReadFvSchemesFile
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function read fvSchemes file
%--------------------------------------------------------------------------

fprintf('\nReading fvSchemes file ...\n');

global Region;

caseDirectoryPath = cfdGetCaseDirectoryPath;

% Read fvSchemes

fvSchemesFileDirectory = [caseDirectoryPath, filesep, 'system', filesep, 'fvSchemes'];

% Check if "fvSchemes" exists
if exist(fvSchemesFileDirectory, 'file')~=2
    error('\n%s\n','"fvSchemes" file is not found in "~foamDirectory', filesep, 'system"');
end

% Read dictionary
fvSchemesDict = cfdReadFoamDictFile(fvSchemesFileDirectory);

% Read entries
Region.foamDictionary.fvSchemes.ddtSchemes = fvSchemesDict.ddtSchemes;
Region.foamDictionary.fvSchemes.gradSchemes = fvSchemesDict.gradSchemes;
Region.foamDictionary.fvSchemes.divSchemes = fvSchemesDict.divSchemes;
Region.foamDictionary.fvSchemes.laplacianSchemes = fvSchemesDict.laplacianSchemes;
Region.foamDictionary.fvSchemes.interpolationSchemes = fvSchemesDict.interpolationSchemes;
Region.foamDictionary.fvSchemes.snGradSchemes = fvSchemesDict.snGradSchemes;

% Set time scheme
if ~strcmp(fvSchemesDict.ddtSchemes.default, 'steadyState')
    Region.STEADY_STATE_RUN = false;
end