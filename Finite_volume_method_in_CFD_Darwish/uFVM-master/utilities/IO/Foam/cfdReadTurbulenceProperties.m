function cfdReadTurbulenceProperties
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads turbulence properties from
%   "constant',filesep,'turbulenceProperties" file
%--------------------------------------------------------------------------

global Region;

caseDirectoryPath = cfdGetCaseDirectoryPath;

turbulencePropertiesFile = [caseDirectoryPath, filesep, 'constant', filesep, 'turbulenceProperties'];

% Check if "cfdTransportProperties" exists
if exist(turbulencePropertiesFile, 'file')~=2
    Region.foamDictionary.turbulenceProperties.turbulence = 'off';
    Region.foamDictionary.turbulenceProperties.RASModel = 'laminar';
    return;
end

fprintf('\n\nReading Turbulence Properties ...\n');

% Open field in read mode
fid = fopen(turbulencePropertiesFile, 'r');

% get simulation type
simulationType = cfdGetKeyValue('simulationType', 'string', fid);
simulationTypeDetails = cfdReadBlockData(simulationType, turbulencePropertiesFile);
for iDetail=1:size(simulationTypeDetails, 1)
    Region.foamDictionary.turbulenceProperties.(simulationTypeDetails{iDetail, 1}) = simulationTypeDetails{iDetail, 2};    
end
