function x=waldrnd(mu,lambda)
% Wald(mu,lambda) generator (Algorithm 4.65)
% vectors mu and lambda have to be of the same size 

x = nan(size(mu));
for i = 1:length(mu)
    m=mu(i); lam=lambda(i);
    Y = randn^2;
    Z = m + m^2*Y/(2*lam) + m/(2*lam)*sqrt(4*m*lam*Y + m^2*Y^2);
    if rand < m/(m + Z)
        x(i) = Z;
    else
        x(i) = m^2/Z;
    end
end