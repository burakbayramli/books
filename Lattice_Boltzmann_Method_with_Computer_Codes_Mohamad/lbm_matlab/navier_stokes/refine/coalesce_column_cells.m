function phi_c = coalesce_column_cells(phi_i)
% Coalesces the (fine) interface column to (coarse) phi_c.
% Refinement level differs only by one between the 2 grids.
% D2Q9
% phi_c and phi_f are 2d matrices.

phi_c = 0.25 * (...
    phi_i(1:2:end,1) + phi_i(1:2:end,2)...
    + phi_i(2:2:end,1) + phi_i(2:2:end,2) );