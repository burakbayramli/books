function [y] = forw_tri(f,bet);
n=length(f);
y=zeros(n,1);
y(1)=f(1);
for i=2:n;
    y(i) = f(i)- bet(i)*y(i-1);
end

        