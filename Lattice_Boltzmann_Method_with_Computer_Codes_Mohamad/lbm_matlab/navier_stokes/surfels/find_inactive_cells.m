function inactive = find_inactive_cells(cut_cells, nodes)
% Assumes cut cavity in bottom left-hand corner.
% returns inactive(j) = i of the maximum-i cell that is inactive.

inactive = nodes*ones(nodes,1);
maxj = 0;
for c = cut_cells
    if c.j > maxj
        maxj = c.j;
    end
    if c.i < inactive(c.j)
        inactive(c.j) = c.i;
    end
end
inactive = inactive(1:maxj)-1;
