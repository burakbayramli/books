function cindep = condindep(A,X,Y,Z)
%CONDINDEP Conditional Independence check using graph of variable interactions
% cindep = condindep(A,X,Y,Z)
%
% A is the adjacency matrix (directed or undirected)
% For sets of variables X,Y,Z specified as vectors:
% return 1 if p(X,Y|Z)=p(X|Z)p(Y|Z)
% 
% Z may be empty, Z=[]
U=[X Y Z];
N=size(A,1);
if mean(mean(abs(A-A')))>0 % DAG
	% remove non ancestral nodes from the graph
	aset=ancestors(U,A); % ancestral set of nodes
	rnodes=setdiff(1:N,[aset U]); % non ancestral nodes
	A(rnodes,:)=0; A(:,rnodes)=0; % remove from graph
	% moralise the remaining graph:
	for i=1:N
		parents=find(A(:,i));
		A(parents,parents)=1; % add links between parents
	end
	A=real((A+A')>0); A=A-diag(diag(A));
end
% See if there is a path from X to Y, not via Z: to do this remove Z from the graph:
A(Z,:)=0; A(:,Z)=0;
% now find the connections between X and Y (naively achieved by taking power of adjacency matrix
% -- include diagonal connections otherwise even-odd path-length problems arise)
A=A+eye(N); A=real(A^N>0); 
A=A-diag(diag(A)); % adjacency matrix of possible paths from X to Y, avoiding Z
cindep=0;
if isempty(find(A(X,Y))) % if no path from X to Y avoiding Z, then indep
	cindep=1;
end