function nameout = checkNfix(filename)
% This functions checks that the .mat ending is in the filene

[a b]=size(filename);

if b>3
suffix=filename(end-3:end);
else
    nameout=[filename '.mat'];
    return
end
    
if strcmp(suffix,'.mat')
    return
elseif strcmp(suffix,'.MAT')
    return
else
    nameout=[filename '.mat'];
    return
end