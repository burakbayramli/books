classdef SWBotTop < Source.SourceBase
%SWBotTop   Bottom topography source term for the shallow water system
	
	properties(Access = private)
		model
		mesh
	end
	
	properties
		b		% Bottom topography function
		bEval	% Bottom topography, evaluated in each cell
	end
	
	
	methods
		% SWBotTop()
		% 'b' should be a function b(x), giving the bottom elevation at each
		% point x.
		function o = SWBotTop(b)
			o.b = b;
		end
		
		
		function initialize(o, config)
			if ~isa(config.model, 'Model.SW')
				error('This source term only works for the shallow water equations');
			end
			if ~isa(config.mesh, 'Mesh.Rect')
				error('Bottom topography only implemented for rectangular meshes');
			end
			o.model = config.model;
			o.mesh = config.mesh;
			
			% Evaluate the bottom topography in each cell and apply the boundary
			% conditions to it
			o.bEval = o.mesh.evalFunc(o.b, config.useCellAvg);
			o.bEval = config.bc.updateBoundary(o.bEval, o.mesh);
		end
		
		
		function ret = source(o, U, UR, t, dt)
			h = U(1, :, :);
			ret = zeros(size(U));
			edgeSource = o.model.grav * Utility.avg(h, 2) .* diff(o.bEval, 1, 2);
			ret(2, o.mesh.intX, o.mesh.intY) = -Utility.avg(edgeSource, 2) ./ o.mesh.dx{1};
			if o.mesh.ndims == 2
				edgeSource = o.model.grav * Utility.avg(h, 3) .* diff(o.bEval, 1, 3);
				ret(3, o.mesh.intX, o.mesh.intY) = -Utility.avg(edgeSource, 3) ./ o.mesh.dx{2};
			end
		end
	end
end