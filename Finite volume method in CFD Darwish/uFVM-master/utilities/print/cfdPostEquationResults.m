function cfdPostEquationResults(theEquationName, iteration)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

global Region;

cfdPrintResiduals(theEquationName);

fprintf('|==========================================================================|\n');
cfdPrintCPUTime;

% Write residuals to file
fileID = fopen([cfdGetCaseDirectoryPath,filesep,'convergence',filesep,'convergence.out'], 'a+');

phiResInit = Region.model.(theEquationName).residuals.rmsResidual;

fprintf(fileID, '\n%d\t%f\t%f', ...
    iteration, ...
    Region.time.currentTime, ...
    phiResInit);

fclose(fileID);