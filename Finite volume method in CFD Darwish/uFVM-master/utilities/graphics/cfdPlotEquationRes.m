function cfdPlotEquationRes(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Plot residuals on fly
%--------------------------------------------------------------------------

fileID = fopen([cfdGetCaseDirectoryPath,filesep,'convergence',filesep,'convergence.out'], 'rt');
dataArray = textscan(fileID, '%d\t%f\t%f', 'Delimiter', '\t', 'HeaderLines', 1);
fclose(fileID);

% Return if no data yet is stored
if isempty(dataArray{1})
    return;
end

%
iters = dataArray{1};
currentTime = dataArray{2};
ResInit = dataArray{3};

clf;
semilogy(iters, ResInit, '->c');

xlabel('Global Iterations');
ylabel('Scaled RMS Residuals');
legend(theEquationName);

grid;
axis tight;
set(gca,'ylim',[1e-12 1e2])

pause(0.01);