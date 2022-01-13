function [um,up] = polynomial_reconstruction(u)

% compute u_{j+1/2}^{-} and u_{j+1/2}^{+}
um = -1/6*circshift(u,1) + 5/6*u + 1/3*circshift(u,-1);
up = 1/3*u + 5/6*circshift(u,-1) - 1/6*circshift(u,-2);

return