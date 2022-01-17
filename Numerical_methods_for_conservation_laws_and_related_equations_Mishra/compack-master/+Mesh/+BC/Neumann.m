classdef Neumann
	%NEUMANN   Homogeneous Neumann boundary conditions.
	
	
	methods (Static)
		function U = updateBoundary(U, mesh, t)
			ngc = mesh.ngc;
			nx = mesh.nx;
			ndims = mesh.ndims;
			
			if ndims >= 1
				for m = 1:ngc
					U(:, m, :) = U(:, ngc+1, :);
					U(:, nx(1)+ngc+m, :) = U(:, nx(1)+ngc, :);
				end
			end
			if ndims >= 2
				for m = 1:ngc
					U(:, :, m, :) = U(:, :, ngc+1, :);
					U(:, :, nx(2)+ngc+m, :) = U(:, :, nx(2)+ngc, :);
				end
			end
		end		
	end	
end