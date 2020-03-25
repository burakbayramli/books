% IS_bounds.m
% the matrix E is assumed to be loaded into the workspace
global GRAPH
GRAPH.E=E;
m=size(E,1); % number of edges
p=ones(m,1)*(1-0.1^3);
% list the edges comprising the paths
paths{1}=[1 4 11 20 28];
paths{2}=[3 8 17 26 30];
paths{3}=[2 6 14 23 29];
ell_U=1; % compute normalizing constant
for i=1:size(paths,2)
    P=paths{i};
    ell_U=ell_U*(1-prod(p(P)));
end
% compute IS estimator
N=10^5; ell=nan(N,1);
for i=1:N
    x=f_bar(p,paths);
    ell(i)=(S(x)>1)*ell_U;
end
[mean(ell), std(ell)/mean(ell)/sqrt(N)]

