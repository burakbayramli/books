function x=laplacernd(mu,sigma)
% Laplace(mu,sigma) generator using inversion  
% (Algorithm 4.42)
% vectors mu and sigma have to be of the same size

U=rand(size(mu))-1/2;
x=sign(U).*log(1-2*abs(U));
x=x.*sigma+mu;