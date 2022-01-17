function foo = approxToExact( meshEx, uEx, cellAvgEx )
%APPROXTOEXACT   Converts a reference solution to an "exact solution
%   function" that can be used by calcOOC, calcError, etc.


% 	assert(meshEx.ndims == 1, ...
% 		'approxToExact() only implemented for 1D solutions');
	
	if meshEx.ndims == 2
		% The nearest-point algorithm only works for rectangular 2D grids
		assert(isa(meshEx, 'Mesh.Rect'), ...
			'approxToExact() only implemented for rectangular 2D grids');
	end

	% TODO: Refine this when cellAvg, cellAvgEx == true
	function ret = exact(mesh, t, cellAvg)
		if meshEx.ndims == 1
			assert(mod(meshEx.nx, mesh.nx) == 0, ...
				'n_x of reference solution must be a multiple of n_x of approximate solution');
			ind = Utility.findNearest(mesh.x, meshEx.x);
			ret = uEx(ind);
		else % meshEx.ndims == 2
			indX = Utility.findNearest(mesh.x{1}(:,:,1), meshEx.x{1}(:,:,1));			
			indY = Utility.findNearest(mesh.x{2}(:,1,:), meshEx.x{2}(:,1,:));
			ret = uEx(:, indX, indY);
		end
	end
	foo = @exact;
end