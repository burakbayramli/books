function collect(ss, f, fluid_areas)
% Fills each pgram in surfel with particle collections. 

for s = ss
    for p = s.pgrams
        collect(p, f, fluid_areas);
    end
end