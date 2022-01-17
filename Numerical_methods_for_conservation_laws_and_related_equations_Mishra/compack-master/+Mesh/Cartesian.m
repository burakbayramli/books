classdef Cartesian < Mesh.Rect
%CARTESIAN   Mesh class for uniform Cartesian 1D and 2D meshes.
%
%   See also the documentation for Mesh.MeshBase.
	
	
	
	
	methods
		% CARTESIAN   Constructor for Cartesian mesh class.
		%    mesh = Mesh.Cartesian(xLim, nx) creates a uniform rectangular
		%    (Cartesian) mesh with boundaries xLim and number of grid cells nx.
		%    For example,
		%       mesh = Mesh.Cartesian([0,1], 100);
		%    discretizes the domain [0,1] into 100 grid cells, while
		%       mesh = Mesh.Cartesian([0,1;0,2], [100, 200]);
		%    discretizes the domain [0,1]x[0,2] into 100 cells in x-direction
		%    and 200 cells in y-direction.
		function o = Cartesian(xLim, nx)
			% Check input data
			narginchk(2, 2);
			ndims = length(nx);
			assert(size(xLim, 1) == ndims);
			assert(size(xLim, 2) == 2);
			
			% Create the mesh and initialize the object as a Rect object.
			x = linspace(xLim(1,1), xLim(1,2), nx(1)+1);
			y = 0;
			if ndims == 2
				y = linspace(xLim(2,1), xLim(2,2), nx(2)+1);
			end
			o = o@Mesh.Rect(x, y);
			
			% Recompute mesh sizes, as they are only scalars for uniform
			% meshes.
			for k = 1:o.ndims
				o.dx{k} = diff(xLim(k,:)) / o.nx(k);
			end
		end
	end
end