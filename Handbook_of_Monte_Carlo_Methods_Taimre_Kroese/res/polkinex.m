%polkinex.m
rho=0.75; alpha=0.5;
gamma=((1-rho)/rho*10^(-11))^(-1/alpha)-1;
bar_F=@(x)(1+x).^(-alpha);
N=10^3; Y=nan(N,1);
for i=1:N
    R=1;
    while rand<rho
        R=R+1;
    end;
    if R==1
        val=gamma;
    else
        X=rand(1,R-1).^(-1/alpha)-1;
        S=sum(X);M=max(X); val=max(M,gamma-S);
    end
    % control variable estimator
    Y(i)=R*bar_F(val)+(1/(1-rho)-R)*bar_F(gamma);
end
format long
ell=mean(Y)*rho
RE = std(Y)/sqrt(N)/ell
