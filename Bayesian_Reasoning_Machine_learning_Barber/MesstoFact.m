function messnum = MessToFact(potnums,FG)
%MESSTOFACT Returns the message numbers that connect into factor potential
% MessToFact(potnums,FG)
% potnums is a vector of potential numbers
% FG is a Factor Graph
% see also FactorConnectingVariable.m, FactorGraph.m
V=min(find(FG(1,:)))-1; % variables are first in the order
facts=potnums+V;
messnum=[];
for f=facts
    messnum = [messnum full(FG(find(FG(:,f)),f))];
end
messnum=unique(messnum);