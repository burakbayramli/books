classdef surfel < handle
    properties
        pgrams          % the pgram handles associated with this surfel.
        startp          % the coordinates of the start point.
        endp            % the coordinates of the end point.  
        segment         % startp to endp.
        normal          % vector normal to segment.
        lattice_indices % the indices of lattice directions (c) 
                        % relevant to this surfel.             
                        % lattice velocity components to use on wall 
                        % (pointing INTO wall FROM fluid domain.).
%         lattice_vectors % the lattice vectors relevant to this surfel.
                        % corresponding to lattice_indices.
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
        function obj = surfel(startp, endp, dh, dt)
            % assumes a lattice grid, with uniform spacing dh, 
            % so that the touched cells can be determined from segment
            % defined by startp and endp.
            obj.startp = startp;
            obj.endp = endp;
            obj.segment = endp - startp;
            % here we assume the normal is 90 clockwise to input segment.
            obj.normal = [-obj.segment(1); obj.segment(2)];
            % Determine the valid lattice vectors.
            valid_lattice_directions(obj);
            % Now let us (newly) make the pgrams associated with this
            % surfel.
            obj.pgrams = []; % zeros(length(obj.lattice_indices),1);
            for k = 1:length(obj.lattice_indices)
                extrusion = -dh*obj.c(obj.lattice_indices(k),:)';
                % obj.pgrams(k) = pgram(obj.startp, obj.segment, extrusion, dh);
                obj.pgrams = [obj.pgrams, ...
                    pgram(obj.startp, obj.segment, extrusion, ...
                        obj.lattice_indices(k), dh)];
            end
        end
    end
    methods ( Access = private )
        function valid_lattice_directions(obj)
            valid = zeros(9,1);
            for k = 1:9
                valid(k) = dot(obj.normal,obj.c(k,:)) < -eps;
            end
            obj.lattice_indices = find(valid==1); % the components relevant to wall.
%             obj.lattice_vectors = obj.c( valid == 1 , : );
        end
    end
end