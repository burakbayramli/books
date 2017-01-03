function l=lengthcell(incell)
%LENGTHCELL Length of each cell entry
% l=length(incell)
if iscell(incell)
    for v=1:length(incell)
        l(v) = length(incell{v});
    end
else
    l=1;
end