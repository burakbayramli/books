function cfdReadGravity
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function reads g from "constant',filesep,'g" file
%--------------------------------------------------------------------------
global Region;

caseDirectoryPath = cfdGetCaseDirectoryPath;

gFile = [caseDirectoryPath, filesep, 'constant', filesep, 'g'];

% Check if "g" exists
if exist(gFile, 'file')~=2
    Region.buoyancy = false;
    return;
end

fprintf('\nReading Gravitational Properties ...\n');

% Open field in read mode
fid = fopen(gFile, 'r');

% Collect gravity attributes
dimensions = cfdGetKeyValue('dimensions', 'dimensions', fid);
gValue     = cfdGetKeyValue('value', 'cfdScalarList', fid);

% Store in foam data base
Region.foamDictionary.g = {};
Region.foamDictionary.g.dimensions = dimensions;
Region.foamDictionary.g.value = gValue;

% Activate Buoyancy
if norm(gValue)>0
    Region.buoyancy = true;
end


