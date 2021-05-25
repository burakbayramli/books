clear all
close all
clc

% Print functions names
system('find . -iname ''*.m'' > log');

% Read generated paths file
fid = fopen('log', 'r');
pathsList = cell(0,1);
while ~feof(fid)
    tline = fgetl(fid);
    pathsList{end+1,1} = tline;
end

% Change function names in all files
for iPath=1:length(pathsList)
    directory = dir(pathsList{iPath});
    currentFunctionName = directory.name;
    newFunctionName = [lower(currentFunctionName(4)), currentFunctionName(5:end)];
    findAndReplace(pathsList, currentFunctionName(1:end-2), newFunctionName(1:end-2));
end

% Change file name
for iPath=1:length(pathsList)
    directory = dir(pathsList{iPath});
    currentFunctionName = directory.name;
    newFunctionName = [lower(currentFunctionName(4)), currentFunctionName(5:end)];
    newFullFunctionPath = [fileparts(pathsList{iPath}), '/', newFunctionName];
    movefile(pathsList{iPath}, newFullFunctionPath);
end