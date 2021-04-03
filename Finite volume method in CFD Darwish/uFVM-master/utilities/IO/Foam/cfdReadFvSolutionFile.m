function cfdReadFvSolutionFile
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function read fvSolution file
%--------------------------------------------------------------------------

fprintf('\nReading fvSolution file ...\n');

global Region;

caseDirectoryPath = cfdGetCaseDirectoryPath;

fvSolutionFileDirectory = [caseDirectoryPath, filesep, 'system', filesep, 'fvSolution'];

% Check if "fvSolution" exists
if exist(fvSolutionFileDirectory, 'file')~=2
    error('\n%s\n','"fvSolution" file is not found in "~foamDirectory', filesep, 'system"');
end

% Read dictionary
fvSolutionDict = cfdReadFoamDictFile(fvSolutionFileDirectory);

% Store and manage dictionary in data base
% Solvers
fieldNamesToSolve = fieldnames(fvSolutionDict.solvers);
for iField=1:length(fieldNamesToSolve)
    fieldName = fieldNamesToSolve{iField};
    
    % Store in data base
    keys = fieldnames(fvSolutionDict.solvers.(fieldName));
    for iEntry=1:length(keys)
        key = keys{iEntry};
        value = fvSolutionDict.solvers.(fieldName).(key);
        if strcmp(key, 'solver') || strcmp(key, 'preconditioner') || strcmp(key, 'smoother')
            Region.foamDictionary.fvSolution.solvers.(fieldName).(key) = value;
        else
            Region.foamDictionary.fvSolution.solvers.(fieldName).(key) = eval(value);
        end
    end
    
    % Additional default settings
    if ~isfield(Region.foamDictionary.fvSolution.solvers.(fieldName), 'maxIter')
        Region.foamDictionary.fvSolution.solvers.(fieldName).maxIter = 20;
    end
    
    if strcmp(Region.foamDictionary.fvSolution.solvers.(fieldName).solver, 'GAMG')
        if ~isfield(Region.foamDictionary.fvSolution.solvers.(fieldName), 'nPreSweeps')
            Region.foamDictionary.fvSolution.solvers.(fieldName).nPreSweeps = 0;
        end
        
        if ~isfield(Region.foamDictionary.fvSolution.solvers.(fieldName), 'nPostSweeps')
            Region.foamDictionary.fvSolution.solvers.(fieldName).nPostSweeps = 2;
        end
        
        if ~isfield(Region.foamDictionary.fvSolution.solvers.(fieldName), 'nFinestSweeps')
            Region.foamDictionary.fvSolution.solvers.(fieldName).nFinestSweeps = 2;
        end
    end
end

% SIMPLE control
entryNames = fieldnames(fvSolutionDict.SIMPLE);
for iEntry=1:length(entryNames)
    if ~isstruct(fvSolutionDict.SIMPLE.(entryNames{iEntry}))
        if strcmp(entryNames{iEntry}, 'pRefCell')
            Region.foamDictionary.fvSolution.SIMPLE.(entryNames{iEntry}) = eval(fvSolutionDict.SIMPLE.(entryNames{iEntry})) + 1;
        else
            Region.foamDictionary.fvSolution.SIMPLE.(entryNames{iEntry}) = eval(fvSolutionDict.SIMPLE.(entryNames{iEntry}));
        end
    else
        % Residual control
        if strcmp(entryNames{iEntry}, 'residualControl')
            fieldNames = fieldnames(fvSolutionDict.SIMPLE.residualControl);
            for iField=1:length(fieldNames)
                fieldName = fieldNames{iField};
                if ~isempty(find(strcmp(fieldNamesToSolve,fieldName)))
                    Region.foamDictionary.fvSolution.SIMPLE.residualControl.(fieldName) = eval(fvSolutionDict.SIMPLE.residualControl.(fieldName));
                else
                    Region.foamDictionary.fvSolution.SIMPLE.residualControl.(fieldName) = 1e-6;
                end
            end
        end
    end
end

% Default reference cell index and value
if ~isfield(Region.foamDictionary.fvSolution.SIMPLE, 'pRefCell')
    Region.foamDictionary.fvSolution.SIMPLE.pRefCell = 1;
end
if ~isfield(Region.foamDictionary.fvSolution.SIMPLE, 'pRefValue')
    Region.foamDictionary.fvSolution.SIMPLE.pRefValue = 0;
end

% Relaxation factors
if isfield(fvSolutionDict, 'relaxationFactors')
    % Equations
    if isfield(fvSolutionDict.relaxationFactors, 'equations')
        fieldNames = fieldnames(fvSolutionDict.relaxationFactors.equations);
        for iField=1:length(fieldNames)
            fieldName = fieldNames{iField};
            if ~isempty(find(strcmp(fieldNamesToSolve,fieldName)))
                Region.foamDictionary.fvSolution.relaxationFactors.equations.(fieldName) = eval(fvSolutionDict.relaxationFactors.equations.(fieldName));
            else
                Region.foamDictionary.fvSolution.relaxationFactors.equations.(fieldName) = 1.0;
            end
        end
    end
    
    % Fields
    if isfield(fvSolutionDict.relaxationFactors, 'fields')
        fieldNames = fieldnames(fvSolutionDict.relaxationFactors.fields);
        for iField=1:length(fieldNames)
            fieldName = fieldNames{iField};
            if ~isempty(find(strcmp(fieldNamesToSolve,fieldName)))
                Region.foamDictionary.fvSolution.relaxationFactors.fields.(fieldName) = eval(fvSolutionDict.relaxationFactors.fields.(fieldName));
            else
                Region.foamDictionary.fvSolution.relaxationFactors.fields.(fieldName) = 1.0;
            end
        end
    end
end
