function [bmin, bmax, imin, imax] = pgram_bounds(p0,v1,v2,dh)
% returns the bottom left and top right coordinates of the 
% cell points that make a (rectangular) box that enclose a pgram.
% p0,v1,v2: describes the pgram.
% dh: lattice cell dimension.
% imin: the indices of the cell whose bottom left corner is bmin.
% imax: the indices of the cell whose top right corner is bmax.

coordinate_shift = [-dh/2, -dh/2];
% coordinate_shift = [0,0];

p0 = reshape(p0,[1,2]);
v1 = reshape(v1,[1,2]);
v2 = reshape(v2,[1,2]);

% First construct all vertices in the pgram.
p1 = p0 + v1;
p2 = p0 + v2;
p3 = p0 + v1 + v2;

points = [p0;p1;p2;p3];
mins = min(points,[],1);
maxs = max(points,[],1);
bmin = floor((mins-coordinate_shift+eps)/dh)*dh + coordinate_shift;
bmax = ceil((maxs-coordinate_shift-eps)/dh)*dh + coordinate_shift;

imin = round((bmin - coordinate_shift) / dh + 1);
imax = round((bmax - coordinate_shift) / dh);

