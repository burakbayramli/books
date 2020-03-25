function x=mcmc(x,gam)
 
global GRAPH 
sig=GRAPH.sig;

m=length(x); 

%#############################

for iter=1:50
    d=randn(1,m); d=d/norm(d); % sample direction
    lam=-x*d'+randn*sig(1);
    y=x+lam*d; % make proposal
    
    if elite(y,gam)
        x=y;  
    end
end
