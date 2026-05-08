classdef touched_cell
    properties
        surface_normal          % vector of surface normal that points into the fluid domain.
        lattice_indices         % the BOUNCEBACK lattice velocity indices that need to be advected from this cell.
        i                       % i index of this touched cell.
        j                       % j index of this touched cell.
        fluid_area              % the area of this cell that is present in the fluid domain.
        pgrams                  % handles of the pgrams that touch this cell.
        overlap_areas           % overlap areas with this cell, corresponding to pgrams / lattice_indices.
        non_overlap_area        % area that is not overlapped with the k surfel.
        distributed_particles   % particles distributed to this cell from pgrams.
        nonoverlap_ratios       % corresponds with lattice_indices. used to scale advection particles. ratio of cell area that is not overlapped with pgrams.
        bi                      % corresponds with lattice indices. neighbouring cell in the bounced direction.
        bj                      % corresponds with lattice indices. neighbouring cell in the bounced direction.
        advection_scales         % corresponds with lattice indices.  
    end
    properties (Constant)
        c = [0, 0; ...
            1, 0; ...
            0, 1; ...
            -1, 0; ...
            0, -1; ...
            1, 1; ...
            -1, 1; ...
            -1, -1; ...
            1, -1; ...
            ];      
    end
    methods
        function obj = touched_cell(i,j,fluid_area,surface_normal)
            obj.i = i;
            obj.j = j;
            obj.fluid_area = fluid_area;
            obj.surface_normal = surface_normal;
            obj.lattice_indices = [];
            for k = 1:9
                if dot(obj.surface_normal, obj.c(k,:)) > eps
                    obj.lattice_indices = [obj.lattice_indices, k];
                end
            end
        end
%         function fa = advected_particles(obj, f)
%             fa = f(obj.j,obj.i,obj.k) * obj.non_overlap_area;
%         end
%         function [fc, pgram_indices] = collected_particles(obj, f, pgrams)
%             fc = f(obj.j,obj.i,obj.k) * obj.overlap_areas;
%             pgram_indices = pgrams;
%         end
    end
end