function pot = eyepot(pot,domain)
%EYEPOT Return a unit potential
% pot = eyepot(pot,domain)
% return a unit potential over the variables in pot
nstates=lengthcell(domain);
for p=1:length(pot);
	for i=1:length(pot(p).variables)
		pot(p).domain{i}=domain{pot(p).variables(i)};
	end
	pot(p).varnames=repmat({''},1,length(pot(p).variables));
	pot(p).table=ones(prod(nstates(pot(p).variables)),1);
end
pot=orderpotfields(pot);