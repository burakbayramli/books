function x=gamrnd_ahrens(alpha)
% Gamma(alpha,1) generator using Ahrens' method
% Algorithm 4.34
b=alpha-1; A=alpha+b; s=sqrt(A);
flag=0;
while flag==0
    t=s*tan(pi*(rand-0.5));
    x=b+t;
    flag=(x>0)&&(rand<exp(b*log(x/b)-t+log(1+t^2/A)));
end