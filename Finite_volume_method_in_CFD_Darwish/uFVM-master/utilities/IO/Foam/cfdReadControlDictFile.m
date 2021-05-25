function cfdReadControlDictFile
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function read controlDict file
%--------------------------------------------------------------------------
%
global Region;

caseDirectoryPath = cfdGetCaseDirectoryPath;

fprintf('\nReading controlDict file ...\n');

controlDictFileDirectory = [caseDirectoryPath, filesep, 'system', filesep, 'controlDict'];

% Check if "controlDict" exists
if exist(controlDictFileDirectory, 'file')~=2
    error('\n%s\n','"controlDict" file is not found in "~foamDirectory', filesep, 'system"');
end

% Read dictionary
controlDict = cfdReadFoamDictFile(controlDictFileDirectory);

% Read entries
Region.foamDictionary.controlDict.application   = controlDict.application;
Region.foamDictionary.controlDict.startFrom     = controlDict.startFrom;
Region.foamDictionary.controlDict.startTime     = eval(controlDict.startTime);
Region.foamDictionary.controlDict.stopAt        = controlDict.stopAt;
Region.foamDictionary.controlDict.endTime       = eval(controlDict.endTime);
Region.foamDictionary.controlDict.deltaT        = eval(controlDict.deltaT);
Region.foamDictionary.controlDict.writeControl  = controlDict.writeControl;
Region.foamDictionary.controlDict.writeInterval = eval(controlDict.writeInterval);
Region.foamDictionary.controlDict.purgeWrite    = eval(controlDict.purgeWrite);
