function [i, j] = cell_point(point,dh)
% returns the i,j index of the cell that contains point.
% for lid driven cavity: assumes points on boundary.

coordinate_shift = [-dh/2, -dh/2];
bmin = floor((point-coordinate_shift)/dh)*dh + coordinate_shift;
imin = round((bmin - coordinate_shift) / dh + 1);
i = imin(1);
j = imin(2);

