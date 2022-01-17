classdef Neumann
%NEUMANN   Homogeneous Neumann boundary conditions for triangular meshes
	
	
	methods (Static)
		function U = updateBoundary(U, mesh, t)
			[border, ghosts, n] = mesh.getGhostcells();
			U(:,ghosts) = U(:,border);
		end
	end
end