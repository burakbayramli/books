function x=mcmc(x,gam)
 
k=length(x)-1; 
  
for iter=1:10
    d=randn(1,k); d=d/norm(d); % sample direction
    lam=-x(1:k)*d'+randn*10;
    y=x(1:k)+lam*d; % make proposal
    
    if S([y,x(k+1)])>=gam
        x(1:k)=y;  
    end
end
x(k+1)=rand*min(exp(-gam+(S(x)+log(x(k+1)))),1) ;








 























