function cfdReplaceFunctionName(functionName, newFunctionName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function replaces the function name in directory and in all files
%   where it is used
%--------------------------------------------------------------------------

% Change file name
for iPath=1:length(paths)
    directory = dir(paths{iPath});
    if strcmp([functionName,'.m'], directory.name)
        movefile(paths{iPath},[fileparts(paths{iPath}), filesep, newFunctionName,'.m']);
        break;
    end    
end



% % Read generated paths file
% fid = fopen(fileName, 'r');
% pathsList = cell(0,1);
% while ~feof(fid)
%     tline = fgetl(fid);
%     pathsList{end+1,1} = tline;
% end


