function cfdInitDirectories(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function create the required folders (convergence, etc)
%--------------------------------------------------------------------------

if exist([cfdGetCaseDirectoryPath,filesep,'convergence'],'dir')~=7
    mkdir([cfdGetCaseDirectoryPath,filesep,'convergence']);
end

if ~isempty(varargin)
    theEquationName = varargin{1};
    fileID = fopen([cfdGetCaseDirectoryPath,filesep,'convergence',filesep,'convergence.out'], 'w');
    fprintf(fileID, '%s\t%s\t%s', 'noIter','Time[s]',[theEquationName, 'ResInit']);    
else
    fileID = fopen([cfdGetCaseDirectoryPath,filesep,'convergence',filesep,'convergenceUp.out'], 'w');
    fprintf(fileID, '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s', 'noIter','Time[s]','UxResInit','UyResInit','UzResInit','pResInit','kResInit','epsilonResInit','omegaResInit','TResInit');
end

fclose(fileID);