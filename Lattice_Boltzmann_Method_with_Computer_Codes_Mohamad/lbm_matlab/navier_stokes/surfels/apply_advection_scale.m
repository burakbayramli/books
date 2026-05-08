function f = apply_advection_scale(f,tc)

for tck = 1:length(tc)
    for k = 1:length(tc(tck).bi)
        f(tc(tck).j,tc(tck).i,tc(tck).lattice_indices(k)) = ...
            f(tc(tck).j,tc(tck).i,tc(tck).lattice_indices(k)) ...
            * tc(tck).advection_scales(k);
    end
end