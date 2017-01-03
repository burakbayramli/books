function [messnum fact]= mess2var(vars,FG)
%MESS2VAR Returns the message numbers that connect in to variables v
% [messnum fact]=mess2var(vars,FG)
% vars is a vector of variables
%
% messnum(i) is the message number from factor fact(i) to vars(i)
% FG is a Factor Graph
% see also mess2fact.m, FactorConnectingVariable.m, FactorGraph.m

V=min(find(FG(1,:)))-1; % variables are first in the order
messnum=[]; fact=[];
for v=vars
    tmp=FG(:,v); tmp=tmp(:)'; tmp2=tmp>0;
    messnum = [messnum full(tmp(tmp2))];
    fact=[fact full(find(tmp2))-V];
end