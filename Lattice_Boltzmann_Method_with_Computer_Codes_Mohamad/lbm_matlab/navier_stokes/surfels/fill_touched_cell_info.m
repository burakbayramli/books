function [tc, overlap_areas] = fill_touched_cell_info(tc, ss, nodes, dh)
% from the information in ss, fill in missing info in touched cells (tc).

% Get the overlap areas.
overlap_areas = zeros(nodes,nodes,9);
for s = ss
    for p = s.pgrams
        li = p.lattice_index;
        bb = bounceback_components(li);
        for k = 1:length(p.celli)
            i = p.celli(k);
            j = p.cellj(k);
            a = p.overlap_areas(k);
            overlap_areas(j, i, bb) = overlap_areas(j, i, bb) + a;
        end
    end
end

% Non overlap ratios
for tck = 1:length(tc)
%     oa = overlap_areas(c.j,c.i,bb);
%     oa = overlap_areas(c.j,c.i,c.lattice_indices);
%     c.overlap_areas = overlap_areas(c.j,c.i,c.lattice_indices);
    for k = tc(tck).lattice_indices
        tc(tck).overlap_areas = [tc(tck).overlap_areas, ...
            overlap_areas(tc(tck).j,tc(tck).i,k)]; 
    end
    tc(tck).nonoverlap_ratios = 1 - tc(tck).overlap_areas ...
        / tc(tck).fluid_area;
end

% determine bounceback neighbour indices.
for tck = 1:length(tc)
    tc(tck).bi = [];
    tc(tck).bj = [];
    for k = 1:length(tc(tck).lattice_indices)
        li = tc(tck).lattice_indices(k);
        di = 0;
        dj = 0;
        if li == 2 || li == 6 || li == 9
            di = 1;
        end
        if li == 4 || li == 7 || li == 8
            di = -1;
        end
        if li == 3 || li == 6 || li == 7
            dj = 1;
        end
        if li == 5 || li == 8 || li == 9
            dj = -1;
        end
        tc(tck).bi(k) = tc(tck).i + di;
        tc(tck).bj(k) = tc(tck).j + dj;
    end
end

% initialize easy-access neighbour areas
fluid_areas = zeros(nodes,nodes);
for tck = 1:length(tc)
    fluid_areas( tc(tck).j, tc(tck).i ) = tc(tck).fluid_area;
end

% now determine the scaling factors for advection.
% scaling just before streaming.
for tck = 1:length(tc)
    tc(tck).advection_scales = [];
    fa = tc(tck).fluid_area;
    for k = 1:length(tc(tck).bi)
        ni = tc(tck).bi(k);
        nj = tc(tck).bj(k);
        na = fluid_areas(nj,ni);
%         li = tc(tck).lattice_indices(k);
        if na <= eps
            na = dh^2;
        end
        tc(tck).advection_scales(k) = fa / na;
        if na ~= dh^2
            disp('unexpected');
        end
    end
end

% % now determine the scaling factors for advection.
% % scaling just before streaming.
% for tck = 1:length(tc)
%     tc(tck).advection_scales = [];
%     fa = tc(tck).fluid_area;
%     for k = 1:length(tc(tck).bi)
%         ni = tc(tck).bi(k);
%         nj = tc(tck).bj(k);
%         na = fluid_areas(nj,ni);
%         li = tc(tck).lattice_indices(k);
%         if na <= eps
%             na = dh^2;
%         end
%         if overlap_areas(nj, ni, li) > 0
%             tc(tck).advection_scales(k) = tc(tck).nonoverlap_ratios(k) ...
%                 * fa / na;
%         else
%             tc(tck).advection_scales(k) = 1;
%         end
%     end
% end



