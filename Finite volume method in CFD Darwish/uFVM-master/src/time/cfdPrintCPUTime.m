function cfdPrintCPUTime
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function prints the time info
%--------------------------------------------------------------------------

global Region;

Region.time.cpuTime = toc;

fprintf('Total Elapsed Time (s): %f\n',Region.time.cpuTime);
fprintf('\n');
fprintf('\n');