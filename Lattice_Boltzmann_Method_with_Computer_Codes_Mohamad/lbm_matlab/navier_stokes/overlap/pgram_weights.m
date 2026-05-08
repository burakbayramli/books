function weights = pgram_weights(p0,v1,v2,dh)
% Returns a list of row vectors [i,j,area]
% the input pgram has contact area 'area' with cell i,j.

[~, ~, imin, imax] = pgram_bounds(p0, v1, v2, dh);

imin = round(imin);
imax = round(imax);
x0 = -dh/2;
y0 = -dh/2;
di = imax(1) - imin(1);
dj = imax(2) - imin(2);
max_weight_count = di*dj;
weights = zeros(max_weight_count,3);
weight_counter = 1;
for i = imin(1):imax(1)
    for j = imin(2):imax(2)
        minpos = [x0 + dh*(i-1), y0 + dh*(j-1)];
        area = overlap_pgram_cell(p0', v1', v2', minpos, dh);
        if area
            weights(weight_counter,:) = [i,j,area];
            weight_counter = weight_counter + 1;
        end
    end
end
weights = weights(1:weight_counter-1,:);