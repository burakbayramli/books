function x=cauchyrnd(mu,sigma)
% Cauchy(mu,sigma) generator 
% using ration of normals method (Algorithm 4.28)
% vectors mu and sigma have to of the same size

x=sigma.*randn(size(mu))./randn(size(mu))+mu;
