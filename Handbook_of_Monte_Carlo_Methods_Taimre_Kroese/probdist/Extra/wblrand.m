function x=wblrand(alpha,lambda)
% Weib(alpha,lambda) generator via inverse transform method
% (Algorithm 4.66)
% vectors alpha and lambda have to be of the same size

x=(-log(rand(size(alpha)))).^(1./alpha)./lambda;