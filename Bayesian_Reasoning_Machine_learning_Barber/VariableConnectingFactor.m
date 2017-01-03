function [v var2factmess fact2varmess] = VariableConnectingFactor(facts,A)
%VARIABLECONNECTINGFACTOR variables connected to a factor in a factor graph
% [v var2fact fact2var] = VariableConnectingFactor(facts,A)
% inputs:  A is a Factor Graph; facts is a vector of factor indices
% outputs:
% v is the (intersection of) variable node indices that connect to factors facts.
% var2factmess is the list of variable to factor message indices for these facts
% fact2varmess is the list of all messages from facts to v
% see also FactorGraph.m, FactorConnectingVariable.m, demoSumprod.m
V=min(find(A(1,:)))-1; % variables are first in the order
c=1;
for f=facts+V
    if c==1
        v = find(A(:,f));
    else
        v = intersect(v,find(A(:,f)));
    end
    c=c+1;
end
fact2varmess=[];var2factmess=[];
for i=1:length(v)
    fact2varmess=full(union(fact2varmess,A(facts+V,v(i))));
    var2factmess=full(union(var2factmess,A(v(i),facts+V)));
end