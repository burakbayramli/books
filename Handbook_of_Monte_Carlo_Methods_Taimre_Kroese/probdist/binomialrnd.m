function x=binomialrnd(n,p)
% recursive binomial generator
if n<=10
      x=sum(rand(1,n)<p); 
else
    k=ceil(n*p);Y=nbinrnd(k,p);% generate NegBin(k,p)
    T=k+Y;
    if T<=n
        x=k+binomialrnd(n-T,p);
    else
        x=k-binomialrnd(T-n,p);
    end
end   

