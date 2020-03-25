%siegmund.m
mu=-1;N=10^6;gamma=13;W=nan(N,1);
for i=1:N
    S=0;
    while S<gamma
        X=-mu+randn;
        S=S+X; 
    end
    W(i)=exp(2*mu*S);
end
ell_hat=mean(W),RE=std(W)/mean(W)/sqrt(N)
