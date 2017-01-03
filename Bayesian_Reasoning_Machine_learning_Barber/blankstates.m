function domain=blankstates(numstates)
%BLANKSTATES Create empty domain
% domain=blankstates(numstates)
% create empty domain eg: domain([1 3])=blankstates([12 6])
for i=1:length(numstates)
    domain{i} = repmat({''},1,numstates(i));
end