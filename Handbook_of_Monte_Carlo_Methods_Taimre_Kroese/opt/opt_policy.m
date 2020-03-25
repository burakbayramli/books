%opt_policy.m
epsilon=10^(-4);
N=100; rho=0.1; alpha=1; beta=.5; Ne=ceil(N.*rho); %alg. parameters
mu=[100,100]; sig=[100,100]; muhist=mu;sighist=sig; %initialize v
while max(sig)>epsilon
    x=repmat(mu,N,1)+repmat(sig,N,1).*randn(N,2); 
    for k=1:N 
        if (x(k,1)>=0)&(x(k,2)>=x(k,1)) 
            S(k)=f(x(k,1),x(k,2)); %score if policy is feasible
        else
            S(k)=inf; % otherwise apply a penalty
        end
    end
    [S,I]=sort(S); % sort performances
    mu=alpha.*mean(x(I(1:Ne),:))+(1-alpha).*mu; %update means
    sig=beta.*std(x(I(1:Ne),:),1,1)+(1-beta).*sig; %upd. std devs
    muhist=[muhist;mu];sighist=[sighist;sig];
    [mu, sig, S(1),S(Ne)] % Display param. vect. & best and worst
end 
