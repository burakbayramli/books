function [A] = PoissonCstab1D();

% function [A] = PoissonCstab1D();
% Purpose: Set up symmetric Poisson matrix with estabilized central fluxes

Globals1D;
A = zeros(K*Np); g = zeros(K*Np,1);

% Build matrix -- one column at a time
for i=1:K*Np
    g(i) = 1.0;
    gmat = reshape(g,Np,K);
    Avec = PoissonCstabRHS1D(gmat);
    A(:,i) = reshape(Avec,K*Np,1);
    g(i)=0.0;
end
return
