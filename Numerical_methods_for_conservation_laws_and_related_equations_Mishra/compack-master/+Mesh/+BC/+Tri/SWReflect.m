classdef SWReflect
%SWREFLECT   Reflecting boundary conditions for the shallow water equations 
%   on triangular meshes
	
	methods (Static)
		function U = updateBoundary(U, mesh, t)
			[border, ghosts, n] = mesh.getGhostcells();
			U(1, ghosts) = U(1,border);
			hu = U(2:3,border);
			U(2:3, ghosts) = hu - 2*Utility.timesArray(dot(hu, n), n);
		end
	end
end