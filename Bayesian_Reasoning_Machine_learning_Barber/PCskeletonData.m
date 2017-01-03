function [G,S]=PCskeletonData(data,varargin)
%PCSKELETONDATA find a Belief Net skeleton from data
% [G,S]=PCskeletonData(data,varargin)
opts=[]; if nargin==2; opts=varargin{1}; end
a=0.1; opts=setfields(opts,'Uxgz',a,'Uygz',a,'Uz',a,'Uxyz',a);% default Dirichlet parameters
nstates=maxarray(data,2)'; vars=1:length(nstates);
% Find the skeleton using the PC algorithm:
G = ones(length(vars)); G = G -diag(diag(G)); % start with fully connected skeleton
for x=vars; for y=vars; S{x,y}=[]; end;end
for i=0:length(vars)
    if all(neighboursize(G,vars)<i); break; end
    for x=vars
        nb = neigh(G,x);
        for y=nb
            subsets=mynchoosek(setdiff(nb,y),i);
            if isempty(subsets)
                if condindepEmp(data(x,:),data(y,:),[],nstates(x),nstates(y),[],0,opts)
                    G(x,y)=0; G(y,x)=0; % remove the x-y link
                end
            else
                for j=1:size(subsets,1)
                    z = subsets(j,:);
                    if condindepEmp(data(x,:),data(y,:),data(z,:),nstates(x),nstates(y),nstates(z),0,opts)
                        G(x,y)=0; G(y,x)=0; % remove the x-y link
                        S{x,y}=union(S{x,y},z); S{y,x}=S{x,y}; % add this link set
                    end
                end
            end
        end
    end
end