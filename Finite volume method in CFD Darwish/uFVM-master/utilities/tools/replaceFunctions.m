function cfdReplaceFunctions

system('find . -iname ''*.m'' > log.files');

% Change file name
for iPath=1:length(paths)
    directory = dir(paths{iPath});
    if strcmp([functionName,'.m'], directory.name)
        movefile(paths{iPath},[fileparts(paths{iPath}), filesep, newFunctionName,'.m']);
        break;
    end    
end