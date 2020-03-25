function out=S(x)
global Y X
k=length(x)-1;
b=x(1:k)';
u=x(k+1);
out=-Y'*log(1+exp(-X*b))-(1-Y)'*(X*b+log(1+exp(-X*b)))-log(u);