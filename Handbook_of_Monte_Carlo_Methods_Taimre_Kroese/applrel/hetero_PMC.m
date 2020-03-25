% hetero_PMC.m
global GRAPH
GRAPH.E=E;
tab=[];
for iter=1:10
    m=size(E,1);    % number of edges
    n=max(E(:));    % number of nodes
    p=ones(m,1)*(1-0.1^iter);
    lam=-log(1-p'); % repair time rates
    L=sum(lam);
    N=10^5; ell=nan(N,1);
    for i=1:N
        x=-log(rand(1,m))./lam; % sample repair times
        [Sx,crit,perm]=S(x);    % compute S(X)
        % compute rates for convolution
        LAM_perm=L-cumsum([0,lam(perm(1:crit-1))]);
        % compute probability given configuration
        ell(i)=convolution(1,LAM_perm);
    end
    tab=[tab;mean(ell), std(ell)/mean(ell)/sqrt(length(ell))]
end
