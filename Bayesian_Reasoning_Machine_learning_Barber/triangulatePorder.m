function [Atri cl elimset] = triangulatePorder(A,porder)
%TRIANGULATEPORDER Triangulate adjacency matrix A according to a partial ordering.
% [Atri cl] = triangulatePorder(A,porder)
% porder{end} is vector containing the partial ordering of variables that
% must be eliminated first. porder{end-1} contains the variables to be
% eliminated after porder{end}, and so on.
% Returns Atri and a structure of cliques, cl(i).variables
%
% Naive scheme based on recursively eliminating the node with the least neighbours
tmpporder=porder; partcount=length(tmpporder);
Atri=A;  N=size(A,1);
cc=0; % clique counter
elimset=[];
for node=1:N
	% now find the neighbours:
	nn=(N+1)*ones(1,N); % ensures we don't pick eliminated nodes at next iteration
	for i=setdiff(1:N,elimset);	nn(i)=length(find(Atri(i,:))); end
	[a b]=sort(nn);
	c = find(ismember(b,tmpporder{partcount}));
	elim=b(c(1)); % eliminate variable with least neighbours in current partial order 
	tmpporder{partcount}=setdiff(tmpporder{partcount},elim); % remove variable from partial order
	if isempty(tmpporder{partcount}); partcount=partcount-1; end
	neigh = setdiff(find(Atri(elim,:)),elim); % neighbours (not including self)
	cc=cc+1; cl(cc).variables=[elim neigh]; % the clique 
	cl(cc).table=1; % dummy table needed for the uniquepots algorithm
	Atri(elim,:)=0; Atri(:,elim)=0; % remove elim from graph
	Atri(neigh,neigh)=1; % add links between neighbours
	elimset=[elimset elim];
end
for c=1:length(cl)
	family=cl(c).variables;
	Atri(family,family)=1;	
end