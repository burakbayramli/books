function theAlgorithm = cfdGetAlgorithm
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the algorithm from the Region
%--------------------------------------------------------------------------

global Region;

subFieldNames = fieldnames(Region.foamDictionary.fvSolution);

for iField=1:length(subFieldNames)
    subField = subFieldNames{iField};
    if strcmp(subField, 'SIMPLE') || strcmp(subField, 'PIMPLE')
        theAlgorithm = subField;
    end
end