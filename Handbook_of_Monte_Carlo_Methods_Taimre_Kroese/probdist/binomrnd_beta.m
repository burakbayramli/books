function x=binomrnd_beta(n,p)
% recursive binomial generator based on Beta dist.

if n<=10
      x=sum(rand(1,n)<p); 
else
    k=ceil(n*p);Uk=betarnd(k,n+1-k);% generate beta r.v.
    if Uk<p
        x=k+binomrnd_beta(n-k,(p-Uk)/(1-Uk));
    else
        x=k-binomrnd_beta(k-1,(Uk-p)/Uk);
    end
end


