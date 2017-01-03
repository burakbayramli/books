function s=gatherstrings(strcell)
%GATHERSTRINGS gather strings in a string cell arrary c{} together
% s=gatherstrings(strcell)
% eg  c{1}='Bayes'; c{2}='Rules'; gatherstrings(c)
s=[];
for i=1:length(strcell)
    s=[s strcell{i}];
end