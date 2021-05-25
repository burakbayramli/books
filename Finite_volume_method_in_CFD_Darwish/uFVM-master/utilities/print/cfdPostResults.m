function cfdPostResults(iteration)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

global Region;

cfdPostResiduals;

fprintf('|==========================================================================|\n');
cfdPrintCPUTime;

% Write residuals to file
fileID = fopen([cfdGetCaseDirectoryPath,filesep,'convergence',filesep,'convergenceUp.out'], 'a+');

if cfdIsSolveEquation('U')
    UxResInit = Region.model.U.residuals.rmsResidual(1);
    UyResInit = Region.model.U.residuals.rmsResidual(2);
    UzResInit = Region.model.U.residuals.rmsResidual(3);
else
    UxResInit = nan;
    UyResInit = nan;
    UzResInit = nan;    
end

if cfdIsSolveEquation('p')
    pResInit = Region.model.p.residuals.rmsResidual;
else
    pResInit = nan;    
end

if cfdIsSolveEquation('T')
    TResInit = Region.model.T.residuals.rmsResidual;
else
    TResInit = nan;     
end

fprintf(fileID, '\n%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f', ...
    iteration, ...
    Region.time.currentTime, ...
    UxResInit, ...
    UyResInit, ...
    UzResInit, ...
    pResInit, ...
    1, ...
    1, ...
    1, ...
    TResInit);

fclose(fileID);