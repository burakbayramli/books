function [Atri cl cliquesize] = triangulate(A)
%TRIANGULATE Triangulate adjacency matrix A
% [Atri cl cliquesize] = triangulate(A)
% Returns Atri and a structure of cliques, cl(i).variables
% Naive scheme based on recursively eliminating the node with the least  neighbours.
% Contrasts with elimtri.m for which the last node in the elimination sequence is specified.
cl=[]; %A=A-diag(diag(A)); %A=A+eye(size(A,1));
comps=connectedComponents(A); Ncomps=max(comps); % find the connected components
for c=1:Ncomps
    compvars=find(comps==c);
    [Atri(compvars,compvars), cltmp]=triangulateComponent(A(compvars,compvars)); %triangulate each component
    cltmp=changevar(cltmp,1:length(compvars),compvars);
    cl=[cl cltmp];
end
cliquesize=0;for c=1:length(cl); cliquesize(1,c)=length(cl(c).variables); end