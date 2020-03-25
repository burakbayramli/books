function x=conditional_rnd(w,sig,gam)

m=length(w);
x=nan(1,m); c=normcdf(-gam,0,sig);
for j=1:m
   
    if rand<w(j)
        x(j)=randn*sig;
    else
        x(j)=-norminv(rand*c,0,sig); 
    end
    
end