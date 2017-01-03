function [newpot]=ungrouppot(grouppot,group)
%UNGROUPPOT form a potential based on ungrouping variables 
% newpot = ungrouppot(grouppot,group)
% 
% grouppot is a potential defined on grouped variables
% group(i).variables contains the variables in group i
% See also grouppot.m, groupstate.m, demogrouppot.m
newvar=[]; nstates=[];
for i=grouppot.variables
    newvar=[newvar group(i).variables];
    for j=1:length(group(i).variables);
        nstates=[nstates group(i).nstates(j)];
    end
end
newpot.variables=newvar;
newpot.table=reshape(grouppot.table,nstates);