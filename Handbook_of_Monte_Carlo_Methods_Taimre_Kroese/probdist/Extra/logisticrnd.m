function x=logisticrnd(mu,sigma)
% Logistic(mu,sigma) generator using inversion
% Algorithm 4.45
% mu and sigma have to be of the same size
U=rand(size(mu));
x=mu+sigma.*log(U./(1-U));
