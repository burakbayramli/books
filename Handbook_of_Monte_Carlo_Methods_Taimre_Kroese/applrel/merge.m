function LAM_perm=merge(E,lam)
% applies the merge process of Elperin et al. (1991)
% and is used in the script merge_process.m;
% see reference [14] in Handbook of Monte Carlo Methods.
% INPUT: E - matrix with the edge set, so that:
%            E(1,:) is the first edge to be UP
%            E(2,:) is the second edge to be UP
%                          .
%                          .
%                          .
%            E(m,:) is the last edge to be UP
%       lam- vector with the repair time rates
%            corresponding to the edges in E
% OUTPUT: LAM_perm - the set of decreasing
%                    exponential rates used by convolution.m
n=max(E(:)); % number of nodes
m=size(E,1); % number of edges
A=zeros(n);  % incidence matrix
LAM_perm(1)=sum(lam);
for iter=1:m
    e=E(1,:); % which edge is up
    A(e,e)=1; % indicate that the nodes of 'e' are connected
    % find with which other nodes e(1) and e(2) are connected
    y=A(e(1),:) | A(e(2),:);
    A(y,y)=1; % indicate the complete connectivity of the nodes
    struc_func=A(1,n); % if nodes 1 and n are connected, set H(X)=1
    % if structure function is one, exit
    if struc_func , break,  end
    ie=[]; % irrelevant/redundant edges
    for k=1:size(E,1)
        ee=E(k,:);
        if A(ee(1),ee(2)) % check if edge is redundant/irrelevant
            ie=[ie;k]; % keep a list of redundant edges
        end
    end
    % adjust repair time rates due to the redundant edges
    LAM_perm=[LAM_perm,LAM_perm(iter)-sum(lam(ie))];
    E(ie,:)=[]; % remove redundant edges
    lam(ie)=[]; % remove rates associated with 'ie' edges
    % if all edges have been used and H(X)=0, then
    % adjust the repair rates so that unreliability=1 and exit
    if isempty(E)
        LAM_perm=eps;
        break
    end
end











