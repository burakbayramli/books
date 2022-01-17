classdef Dirichlet
%DIRICHLET   Homogeneous Dirichlet boundary conditions for triangular meshes
	
	
	methods (Static)
		function U = updateBoundary(U, mesh, t)
			[border, ghosts, n] = mesh.getGhostcells();
			U(:,ghosts) = 0;
		end
	end
end

