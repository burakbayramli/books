function addpwd
% addpwd  Add the current directory to the path
oldPath = path;
thisDir = pwd;
path(oldPath,thisDir);
fprintf('The following directory has been added to the path\n\t%s\n',thisDir);
