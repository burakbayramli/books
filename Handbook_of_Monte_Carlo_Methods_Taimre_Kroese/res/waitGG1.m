%waitGG1.m
N=10^5;gamma=13;W=nan(N,1);mu=2; lam=1/2;
for i=1:N
    S=0;
    while S<gamma
        X=-log(rand)/lam+log(rand)/mu;
        S=S+X;
    end
    W(i)=exp(-(mu-lam)*S);
end
mean(W),std(W)/mean(W)/sqrt(N)
