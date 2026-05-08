function fluid_areas = fluid_areas_table(cut_cells, nodes, dh)
% Returns the fluid area for every cell as a 2D matrix.
% area is less than dh^2 for cut cells 
% 0 (or dh^2, don't care) for cells out of domain.
% fluid_areas: vector of touched_cell objects, representing the cut cells.

fluid_areas = dh^2 * ones(nodes,nodes);
for c = cut_cells
    fluid_areas(c.j, c.i) = c.fluid_area;
end
