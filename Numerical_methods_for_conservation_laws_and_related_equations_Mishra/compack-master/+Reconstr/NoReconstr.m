classdef NoReconstr < Reconstr.Reconstruct
	%NORECONSTR   No reconstruction -- cell averages are kept as they are.
	
	
	properties (SetAccess = protected)
		ngc = 1;
	end
	
	
	methods
		function UReconstr = reconstruct(obj, U, mesh)
			% Set all values to U
			UReconstr = cell(mesh.ndims, 2);
			for i = 1:mesh.ndims
				UReconstr{i, 1} = U;
				UReconstr{i, 2} = U;
			end
		end
	end	
end