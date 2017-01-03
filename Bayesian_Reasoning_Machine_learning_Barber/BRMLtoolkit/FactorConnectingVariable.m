function [f fact2varmess var2factmess] = FactorConnectingVariable(vars,A)
%FACTORCONNECTINGVARIABLE Factor nodes connecting to a set of variables
% [f fact2varmess var2factmess] = FactorConnectingVariable(vars,A)
% find the intersection of factor indices that connect to variables vars.
% fact2varmess are the message indices connecting the factors to the variable
% var2factmess are the message indices from the variables to the factors
% A is a  Factor Graph 
% vars are the variables
% see also FactorGraph.m, demoSumProd.m
c=1;
for v=vars
	if c==1
		f = find(A(v,:));
	else
		f = intersect(f,find(A(v,:)));
	end
	c=c+1;
end
fact2varmess=[];var2factmess=[];
for fn=f
	fact2varmess=full(union(fact2varmess,A(f,vars)));
	var2factmess=full(union(var2factmess,A(vars,f)));
end
V=min(find(A(1,:)))-1; % variables are first in the order
f=full(f-V);