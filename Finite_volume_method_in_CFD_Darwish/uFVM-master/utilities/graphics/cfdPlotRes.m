function cfdPlotRes
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Plot residuals on fly
%--------------------------------------------------------------------------

fileID = fopen([cfdGetCaseDirectoryPath,filesep,'convergence',filesep,'convergenceUp.out'], 'rt');
dataArray = textscan(fileID, '%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f', 'Delimiter', '\t', 'HeaderLines', 1);
fclose(fileID);

% Return if no data yet is stored
if isempty(dataArray{1})
    return;
end

%
iters = dataArray{1};
currentTime = dataArray{2};
UxResInit = dataArray{3};
UyResInit = dataArray{4};
UzResInit = dataArray{5};
pResInit = dataArray{6};
TResInit = dataArray{10};

clf;

legendStringArray = {};
theEquationNames = cfdGetEquationNames;

for iEquation=1:length(theEquationNames)
    if strcmp(theEquationNames{iEquation}, 'U')
        legendStringArray{end+1} = 'Ux';
        legendStringArray{end+1} = 'Uy';
        legendStringArray{end+1} = 'Uz';
        
        semilogy(iters, UxResInit, '-xr');
        hold on;
        semilogy(iters, UyResInit, '-og');
        hold on;
        semilogy(iters, UzResInit, '-+b');
        hold on;                
    elseif strcmp(theEquationNames{iEquation}, 'p')
        legendStringArray{end+1} = 'p-mass';        
        semilogy(iters, pResInit, '-<k');
    elseif strcmp(theEquationNames{iEquation}, 'T')
        legendStringArray{end+1} = 'T';        
        semilogy(iters, TResInit, '->c');           
    end    
end

xlabel('Global Iterations');
ylabel('Scaled RMS Residuals');
legend(legendStringArray);

grid;
axis tight;
set(gca,'ylim',[1e-6 1e2])

pause(0.01);