%compsum.m
clear all
N=10^4; W=nan(N,1); gamma=10^3;lambda=300;
theta_star=1-(2*lambda/gamma)^(1/3);
for k=1:N
    R=poissrnd((gamma^2*lambda/4)^(1/3));
    S=sum(gamrnd(2,1/(1-theta_star),1,R));
    if S>gamma
        W(k)=exp(-theta_star*S+lambda*((1-theta_star)^(-2)-1));
    else
        W(k)=0;
    end
end
mean(W), std(W)/sqrt(N)/mean(W)
