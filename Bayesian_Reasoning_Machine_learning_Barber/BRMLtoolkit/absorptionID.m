function [jtprob jtutil utilroot]=absorptionID(jtprob,jtutil,infostruct,partialorder)
%ABSORPTIONID Perform full round of absorption on an Influence Diagram
% [jtprob jtutil utilroot]=absorptionID(jtprob,jtutil,infostruct,partialorder) 
% Absorption on an Junction Tree of an Influence Diagram
% see also jtreeID.m
schedule=1:length(jtprob)-1; 
Atree=infostruct.cliquetree; % clique tree
for s=schedule
	% absorb pot(a)-->pot(b)
	pota = s;
	potb = find(Atree(s,:));
    
	% probability update:
	sepvars=intersect(jtprob(pota).variables,jtprob(potb).variables);
	maxsumvars=setdiff(jtprob(pota).variables,sepvars);
	sepprob = maxsumpot(jtprob(pota),maxsumvars,partialorder);    
	jtprob(potb)=multpots([jtprob(potb) sepprob]);
	
	% utility update:
	seputil = maxsumpot(multpots([jtprob(pota) jtutil(pota)]),maxsumvars,partialorder);
	jtutil(potb)=sumpots([jtutil(potb) divpots(seputil,sepprob)]);
end
[probroot, utilroot]=sumpotID(jtprob(end),jtutil(end),[],[],partialorder,0); utilroot=utilroot.table;