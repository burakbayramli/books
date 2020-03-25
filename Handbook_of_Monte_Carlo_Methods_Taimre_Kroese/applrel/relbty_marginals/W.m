function L=W(x,w,sig,gam)
% w is the probability of sampling from nominal for each component

m=length(w); const=normcdf(-gam,0,sig);
N=size(x,1);
L=ones(N,1);
for i=1:N
    for j=1:m
        L(i)=L(i)/(   w(j)+(1-w(j))*(x(i,j)>gam)/const  );
    end
end

