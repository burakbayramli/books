function interface = explode_column_cells(phi_c)
% Explodes the last column of coarse phi_c into 2 columns of double length.
% Refinement level differs only by one between the 2 grids.
% D2Q9
% phi_c is 2d matrix.

[rows, ~] = size(phi_c);
interface = zeros(rows*2, 2);
interface(1:2:end,1) = phi_c(:,end);
interface(2:2:end,1) = phi_c(:,end);
interface(1:2:end,2) = phi_c(:,end);
interface(2:2:end,2) = phi_c(:,end);

