function [x] = back_tri(y,alf,c);
n=length(y);
x=zeros(n,1);
x(n)=y(n)/alf(n);
for i=n-1:-1:1;
    x(i) = (y(i)- c(i)*x(i+1))/alf(i);
end

        