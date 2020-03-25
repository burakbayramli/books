function N=poissrnd_atk(lam)
% Poi(lambda)  generator via Atkinson's method
% Algorithm 4.15
beta=pi/sqrt(3*lam);alpha=lam*beta;
c=0.767-3.36/lam;
k=log(c)-lam-log(beta);
flag=0;
while flag==0
    u=rand;
    x=(alpha-log((1-u)/u))/beta;
    while x<-0.5
        u=rand;
        x=(alpha-log((1-u)/u))/beta;
    end
    N=floor(x+0.5);
    flag=(alpha-beta*x+log(rand/(1+exp(alpha-beta*x))^2))...
        <=(k+N*log(lam)-gammaln(N+1));
end

