function f = scatter(ss, f, fluid_areas, total_overlap_areas)
% Scatters the particle collections in each pgram of each surfel to the
% corresponding cell.
% Have to account for cut cells...

for s = ss
    for p = s.pgrams
        f = scatter(p,f,fluid_areas, total_overlap_areas);
    end
end
    