function [spTree elimseq]=singleparenttree(Atree,varargin)
%SINGLEPARENTTREE From an undirected tree, form a directed tree with at most one parent
%[spTree elimseq]=singleparenttree(Atree,<orient away from this node>)
% Get an elimination elimseq such that each eliminated node has at most 1 parent:
% By default consistently orients away from node 1.
Atree=(Atree+Atree')>0;
if isempty(varargin);
    start=1; %  orient edges away from node 1
else
    start=varargin{1}; % orient away from specified node
end
comps=connectedComponents(Atree); Ncomps=max(comps); elimseq=[]; 
for c=1:Ncomps % loop over connected components
    compvars=find(comps==c);
    if start>length(compvars); compstart=1; else compstart=start; end
    [spTree(compvars,compvars) compelimseq]=singleparentConnectedtree(Atree(compvars,compvars),compstart);
    elimseq=[elimseq compvars(compelimseq)];
end
function [spTree elimseq]=singleparentConnectedtree(Atree,start)
elimseq=start;
N=size(Atree,1); spTree=Atree; v=zeros(N,1); v(start)=1; A=Atree+eye(N);
kids=children(spTree,elimseq); spTree(kids,elimseq)=0;
while prod(v)~=1
    v=real((A*v)>0);
    nodes=setdiff(find(v),elimseq); elimseq=[elimseq nodes(:)'];
    kids=children(spTree,nodes); spTree(kids,nodes)=0;
end