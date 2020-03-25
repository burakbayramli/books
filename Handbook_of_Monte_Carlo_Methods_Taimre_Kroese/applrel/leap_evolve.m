% leap_evolve.m
global GRAPH
GRAPH.E=E;
m=size(E,1);    % number of edges
n=max(E(:));    % number of nodes
p=ones(m,1)*(1-0.1^6);
lam=-log(1-p'); % repair time rates
N=2*10^5; ell=nan(N,1);  gamma=0.1;
for i=1:N
    x=-log(rand(1,m))./lam; % sample repair times
    [Sx,crit,perm]=S(x);    % compute S(X)
    if Sx<gamma
        ell(i)=0;
    else
        idx=find(sort(x)>=gamma,1,'first'); %compute p
        L=sum(lam(perm(idx:end))); % compute rates for conv.       
        LAM_perm=L-cumsum([0,lam(perm(idx:crit-1))]);
        ell(i)=convolution(1-gamma,LAM_perm);
    end
end
mean(ell), std(ell)/mean(ell)/sqrt(length(ell))
