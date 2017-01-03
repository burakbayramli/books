function [G,S]=PCskeletonOracle(A)
%PCSKELETONORACLE Learn the skeleton of a Belief network based on an oracle
%[G,S]=PCskeletonOracle(A)
% A is an adjacency matrix of a DAG
vars=1:size(A,1);
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
                if condindep(A,x,y,[])
                    G(x,y)=0; G(y,x)=0; % remove the x-y link
                end
            else
                for j=1:size(subsets,1)
                    z = subsets(j,:);
                    if condindep(A,x,y,z)
                        G(x,y)=0; G(y,x)=0; % remove the x-y link
                        S{x,y}=union(S{x,y},z); S{y,x}=S{x,y}; % add this link set
                    end
                end
            end
        end
    end
end