function [A,M] = Poisson2D();

% function [A,M] = Poisson2D()
% Purpose: Set up matrix for 2D Poisson equation based on stabilized 
%          internal fluxes on symmetric form

Globals2D;
g = zeros(K*Np,1);
A = spalloc(K*Np, K*Np, 3*Np);  M = spalloc(K*Np, K*Np, 3*Np); 

% Build matrix -- one column at a time
for i=1:K*Np
    g(i) = 1.0;
    gmat = reshape(g,Np,K);
    [Avec,Mvec] = PoissonRHS2D(gmat);
   
    ids = find(Avec); A(ids,i) = Avec(ids);
    ids = find(Mvec); M(ids,i) = Mvec(ids);
    g(i)=0.0;
end
return
