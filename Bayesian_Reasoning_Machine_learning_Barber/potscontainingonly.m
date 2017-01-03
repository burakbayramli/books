function potnums=potscontainingonly(variables,pots)
%POTSCONTAININGONLY Returns those potential numbers that contain only the required variables 
% potnums=potscontainingonly(variables,pots)
[allvars varnames domain]=potvariables(pots);
othervars = setdiff(allvars,variables);
potnums=[];
for p=1:length(pots)
    if prod(1-real(ismember(pots(p).variables, othervars)))
        potnums=[potnums p];
    end
end
