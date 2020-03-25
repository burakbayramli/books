function x=dirichrnd(alpha)
% Dirichlet(alpha) generator using Gamma(a,1) generator
% (Algorithm 4.67)

n=length(alpha)-1;
Y=nan(1,n+1);
for k=1:n+1
    Y(k)=gamrand(alpha(k),1);
end
x=Y(1:n)/sum(Y);
