function [x,w] = gauss(N)
% GAUSS   nodes x (Legendre points) and weights w
%         for Gauss quadrature
%Due to L. N. Trefethen, Spectral Methods in MATLAB, SIAM, Philadelphia, 2000
 
    beta = .5./sqrt(1-(2*(1:N-1)).^(-2)); 
    T = diag(beta,1) + diag(beta,-1); 
    [V,D] = eig(T); 
    x = diag(D); 
    [x,i] = sort(x); 
    w = 2*V(1,i).^2;
end
