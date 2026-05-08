function f = zero_wall_cells(f, wall_cells, ci)
% Meant to be called before scattering.
% omits very first and very last, since these will touch both wall and cut.
% Zeros the relevant distribution function components according to the
%   surfel-incoming components ci.
% Only those cells who touch the wall (and do not get streamed certain
%   components, and therefore have unknown components) are zero'd.

bi = bounceback_components(ci);
for k = 2:size(wall_cells,1)-1
    i = wall_cells(k,1);
    j = wall_cells(k,2);
    f(j,i,bi) = 0;
end