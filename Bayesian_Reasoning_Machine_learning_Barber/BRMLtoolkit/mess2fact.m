function [messnum vars]= mess2fact(facts,FG)
%MESS2FACT Returns the message numbers that connect into factor potential
% [messnum vars]=mess2fact(facts,FG)
% potnums is a vector of potential numbers
% FG is a Factor Graph
%
% messnum(i) contains the message number into the factors facts from variable vars(i)
%
% see also mess2var, FactorConnectingVariable.m, FactorGraph.m
V=min(find(FG(1,:)))-1; % variables are first in the order
factnums=facts+V;
messnum=[]; vars=[];
for f=factnums
    tmp=FG(:,f); tmp=tmp(:)'; tmp2=tmp>0;
    vars=[vars find(tmp2)];
    messnum = [messnum  full(tmp(tmp2))];
end