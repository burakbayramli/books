function x=frechetrnd(alpha)
% Frechet(alpha,0,1) generator

x=(-log(rand(size(alpha)))).^(-1./alpha);