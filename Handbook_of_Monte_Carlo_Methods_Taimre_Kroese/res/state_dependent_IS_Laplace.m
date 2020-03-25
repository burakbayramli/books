%state_dependent_IS_Laplace.m
N=1000; S(1)=0;gamma=4;n=10; % set-up the parameters
for i=1:N
    for k=1:n-1
        % find root for twisting
        theta(k)=fzero(@(t)(  2*t/(1-t^2)...
	-(n*gamma-S(k))/(n+1-k)  ),[-0.999,.999]);
        % sample from twisted density
        if rand<(1+theta(k))/2
            x(k)=-log(rand)/(1-theta(k));
        else
            x(k)=log(rand)/(1+theta(k));
        end
        S(k+1)=S(k)+x(k); % increment the walk
    end
    % compute the likelihood ratio of the path
    W(i)=exp(-x*theta'-sum(log(1-theta.^2)))*...
    (1-cdf_laplace(n*gamma-S(end)));
end
ell=mean(W)
RE=std(W)/ell/sqrt(N)

