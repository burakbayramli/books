function pathsList = cfdUfvmFunctionsList
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns a list of the paths of all the functions of uFVM
%--------------------------------------------------------------------------

p = path; existingMATLABPaths = strsplit(p,':');

uFVMPaths = cell(0);

for iPath=1:length(existingMATLABPaths)
    if contains(existingMATLABPaths{iPath}, [filesep,'uFVM',filesep,'src']) || ...
       contains(existingMATLABPaths{iPath}, [filesep,'uFVM',filesep,'tutorials']) || ...
       contains(existingMATLABPaths{iPath}, [filesep,'uFVM',filesep,'utilities'])
        uFVMPaths{end+1,1} = existingMATLABPaths{iPath};
    end
end



% cfdReplaceFunctionName