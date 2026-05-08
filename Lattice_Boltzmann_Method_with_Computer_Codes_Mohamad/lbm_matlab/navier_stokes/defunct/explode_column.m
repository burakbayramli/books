function phi_f = explode_column(phi_c, phi_f)
% Explodes the last column of coarse phi_c to the first column of phi_f.
% Refinement level differs only by one between the 2 grids.
% D2Q9
% phi_c and phi_f are 2d matrices.

phi_f(1:2:end,1) = phi_c(1:end-1,end);
phi_f(2:2:end,1) = phi_c(2:end,end);
phi_f(1:2:end,2) = phi_c(1:end-1,end);
phi_f(2:2:end,2) = phi_c(2:end,end);

