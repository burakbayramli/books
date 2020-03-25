function I=conditional_pdfs(X,gam)
% X is the MCMC output for the network

[N,m]=size(X);
I=nan(N,m);

for i=1:N
    x=X(i,:);
    for j=1:m
        y=x;
        y(j)=0;
        I(i,j)=S(y)>=gam;
    end
end




