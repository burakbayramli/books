%chibs.m
clear all,clc
N=10^4;
xs=ones(1,5)*8; % x-star
for j=1:4 % number of conditional densities
    pi(j)=0;x=xs;
    for iter=1:N
        for k=j:5
            x(k)=bet(x,k)-log(rand);
        end
        pi(j)=pi(j)+exp(-xs(j)+bet(x,j));
    end
    pi(j)=pi(j)/N;
end
% step 3
pi(5)=exp(-(xs(5)-bet(xs,5)))
% estimator
exp(-sum(xs))/prod(pi)
 
