function x=f_bar(p,paths)
% implements sampling from f_bar
% p is a vector with the corresponding reliabilities
% 'paths' is a cell array of edges describing the paths;

%step 7 and 8
m=length(p);x=zeros(1,m); p=p(:)';
for j=1:size(paths,2)
    P=paths{j}; % paths have to be disjoint!
    x(P)=path_sampling(p(P));
end
idx=find(x==0); % find the edges that are not on the paths
% sample edges not belonging to the paths
x(idx)=-log(rand(1,length(idx)))./(-log(1-p(idx)));
