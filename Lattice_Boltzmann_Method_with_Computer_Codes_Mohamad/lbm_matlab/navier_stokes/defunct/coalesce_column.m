function phi_c = coalesce_column(phi_f, phi_c)
% Coalesces the last column of coarse phi_c to the first column of phi_f.
% Refinement level differs only by one between the 2 grids.
% D2Q9
% phi_c and phi_f are 2d matrices.

phi_c(1,end) = 0.5 * ( phi_f(1,1) + phi_f(1,2) );
phi_c(2:end-1,end) = 0.25 * (...
    phi_f(2:2:end-1,1) + phi_f(3:2:end-1,1)...
    + phi_f(2:2:end-1,2) + phi_f(3:2:end-1,2) );
phi_c(end,end) = 0.5 * ( phi_f(end,1) + phi_f(end,2) );