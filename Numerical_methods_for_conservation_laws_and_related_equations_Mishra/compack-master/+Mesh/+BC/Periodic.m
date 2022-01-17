classdef Periodic
	%PERIODIC   Periodic boundary condition
	
	
	methods (Static)
		function U = updateBoundary(U, mesh, t)
			ngc = mesh.ngc;
			nx = mesh.nx;
			ndims = mesh.ndims;
			
			m = 1:ngc;		% Boundary index
			if ndims >= 1
				U(:, m, :) = U(:, nx(1)+m, :);
				U(:, nx(1)+ngc+m, :) = U(:, ngc+m, :);
			end
			if ndims >= 2
				U(:, :, m, :) = U(:, :, nx(2)+m, :);
				U(:, :, nx(2)+ngc+m, :) = U(:, :, ngc+m, :);
			end
		end		
	end	
end

