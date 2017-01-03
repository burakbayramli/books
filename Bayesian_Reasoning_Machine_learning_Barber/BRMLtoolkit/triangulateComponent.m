function [Atriangulated cl toowide]=triangulateComponent(A,varargin) % triangulate a single component
%TRIANGULATECOMPONENT triangulate a connected component
%[Atriangulated cl toowide]=triangulateComponent(A,<maxtreedwidth>)
% Atriangulated : triangulated graph adjacency matrix
% cl.variables contains the variables in each clique
% toowide : algorithm terminates abruptly if there is a clique exceeding maxtreewidth
maxtreewidth=10e10;
if nargin==2; maxtreewidth=varargin{1}; end
Aleft=sparse(logical(A));
N=size(A,1);
cc=0; cl=[];% clique counter
cl.variables=[]; done=0; toowide=0;
Atriangulated=logical(sparse(N,N)); Aleft=Aleft+eye(N);
while sum(Aleft(:))>0 && ~done
    % check first if what's left is a maximal clique -- if so stop
    nodesleft=find(sum(Aleft));subAleft=Aleft(nodesleft,nodesleft);
    if all(subAleft(:))
        cc=cc+1;
        cl(cc).variables=nodesleft;
        Atriangulated(nodesleft,nodesleft)=1; done=1;
    else
        cc=cc+1;
        elim=argmin(replace(neighboursize(Aleft),0,N)); % node with least neighbours
        neighb=neigh(Aleft,elim);% neighbours (not including self)
        if length(neighb)>maxtreewidth; toowide=1; done=1; end            
        Aleft(elim,:)=0; Aleft(:,elim)=0; % remove elim from graph
        Aleft(neighb,neighb)=1; % add links between neighbours
        Atriangulated([elim neighb],[elim neighb])=1;
        cl(cc).variables=[elim neighb];
    end
end