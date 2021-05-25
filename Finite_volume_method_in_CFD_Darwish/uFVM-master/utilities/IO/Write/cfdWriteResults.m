function cfdWriteResults(currentGlobalIteration)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function writes the results at each write interval
%--------------------------------------------------------------------------

% Get time quantities
runTime = cfdGetCurrentTime;
cpuTime = cfdGetCPUTime;


% Get control settings
foamDict = cfdGetFoamDict;
writeControl = foamDict.controlDict.writeControl;
writeInterval = foamDict.controlDict.writeInterval;
purgeWrite = foamDict.controlDict.purgeWrite;

% Get equation names
theFieldNames = cfdGetFields;

if cfdIsTransient
    if strcmp(writeControl, 'timeStep')
        if rem(runTime, writeInterval)==0
            if ~cfdIsFolderExists(num2str(runTime))
                mkdir(num2str(runTime));
            end
            
            % Write all fields
            for iField=1:length(theFieldNames)
                if strcmp(theFieldNames{iField}, 'DU1') || ...
                        strcmp(theFieldNames{iField}, 'DU2') || ...
                        strcmp(theFieldNames{iField}, 'DU3') || ...
                        strcmp(theFieldNames{iField}, 'DUT1') || ...
                        strcmp(theFieldNames{iField}, 'DUT2') || ...
                        strcmp(theFieldNames{iField}, 'DUT3') || ...
                        strcmp(theFieldNames{iField}, 'pp')
                    continue;
                end
                cfdWriteOpenFoamField(theFieldNames{iField}, runTime);
            end
        end
    elseif strcmp(writeControl, 'runTime')
        if rem(runTime, writeInterval)==0
            if ~cfdIsFolderExists(num2str(runTime))
                mkdir(num2str(runTime));
            end
            
            % Write all fields
            for iField=1:length(theFieldNames)
                if strcmp(theFieldNames{iField}, 'DU1') || ...
                        strcmp(theFieldNames{iField}, 'DU2') || ...
                        strcmp(theFieldNames{iField}, 'DU3') || ...
                        strcmp(theFieldNames{iField}, 'DUT1') || ...
                        strcmp(theFieldNames{iField}, 'DUT2') || ...
                        strcmp(theFieldNames{iField}, 'DUT3') || ...
                        strcmp(theFieldNames{iField}, 'pp')
                    continue;
                end
                cfdWriteOpenFoamField(theFieldNames{iField}, runTime);
            end
        end   
    elseif strcmp(writeControl, 'cpuTime')
        if rem(cpuTime, writeInterval)==0
            if ~cfdIsFolderExists(num2str(cpuTime))
                mkdir(num2str(cpuTime));
            end
            
            % Write all fields
            for iField=1:length(theFieldNames)
                if strcmp(theFieldNames{iField}, 'DU1') || ...
                        strcmp(theFieldNames{iField}, 'DU2') || ...
                        strcmp(theFieldNames{iField}, 'DU3') || ...
                        strcmp(theFieldNames{iField}, 'DUT1') || ...
                        strcmp(theFieldNames{iField}, 'DUT2') || ...
                        strcmp(theFieldNames{iField}, 'DUT3') || ...
                        strcmp(theFieldNames{iField}, 'pp')
                    continue;
                end
                cfdWriteOpenFoamField(theFieldNames{iField}, cpuTime);
            end
        end         
    end
else
    if strcmp(writeControl, 'timeStep')
        if rem(currentGlobalIteration, writeInterval)==0
            if ~cfdIsFolderExists(num2str(runTime))
                mkdir(num2str(runTime));
            end
            
            % Write all fields
            for iField=1:length(theFieldNames)
                if strcmp(theFieldNames{iField}, 'DU1') || ...
                        strcmp(theFieldNames{iField}, 'DU2') || ...
                        strcmp(theFieldNames{iField}, 'DU3') || ...
                        strcmp(theFieldNames{iField}, 'DUT1') || ...
                        strcmp(theFieldNames{iField}, 'DUT2') || ...
                        strcmp(theFieldNames{iField}, 'DUT3') || ...
                        strcmp(theFieldNames{iField}, 'pp')
                    continue;
                end
                cfdWriteOpenFoamField(theFieldNames{iField}, runTime);
            end
        end
    elseif strcmp(writeControl, 'runTime')
        if rem(runTime, writeInterval)==0
            if ~cfdIsFolderExists(num2str(runTime))
                mkdir(num2str(runTime));
            end
            
            % Write all fields
            for iField=1:length(theFieldNames)
                if strcmp(theFieldNames{iField}, 'DU1') || ...
                        strcmp(theFieldNames{iField}, 'DU2') || ...
                        strcmp(theFieldNames{iField}, 'DU3') || ...
                        strcmp(theFieldNames{iField}, 'DUT1') || ...
                        strcmp(theFieldNames{iField}, 'DUT2') || ...
                        strcmp(theFieldNames{iField}, 'DUT3') || ...
                        strcmp(theFieldNames{iField}, 'pp')
                    continue;
                end
                cfdWriteOpenFoamField(theFieldNames{iField}, runTime);
            end
        end
    elseif strcmp(writeControl, 'cpuTime')
        if rem(cpuTime, writeInterval)==0
            if ~cfdIsFolderExists(num2str(cpuTime))
                mkdir(num2str(cpuTime));
            end
            
            % Write all fields
            for iField=1:length(theFieldNames)
                if strcmp(theFieldNames{iField}, 'DU1') || ...
                        strcmp(theFieldNames{iField}, 'DU2') || ...
                        strcmp(theFieldNames{iField}, 'DU3') || ...
                        strcmp(theFieldNames{iField}, 'DUT1') || ...
                        strcmp(theFieldNames{iField}, 'DUT2') || ...
                        strcmp(theFieldNames{iField}, 'DUT3') || ...
                        strcmp(theFieldNames{iField}, 'pp')
                    continue;
                end
                cfdWriteOpenFoamField(theFieldNames{iField}, cpuTime);
            end
        end        
    end
end

% Check the number of written time steps and delete first time steps
writtenTimeSteps = cfdGetTimeSteps;
if purgeWrite~=0 && length(writtenTimeSteps)>purgeWrite+1
    for iTime=2:length(writtenTimeSteps)-2
        rmdir(num2str(writtenTimeSteps(iTime)),'s');
    end
end
