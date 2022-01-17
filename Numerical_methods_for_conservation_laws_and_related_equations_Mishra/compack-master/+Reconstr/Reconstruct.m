classdef Reconstruct < handle
	%RECONSTRUCTION   Abstract base class for all reconstruction methods. 
	
	
	properties (Abstract, SetAccess = protected)
		ngc			% Number of ghost cells needed
	end
	
	properties (Access = protected)
		model
	end

		
	methods (Abstract)
		% reconstruct()
		% Perform spatial reconstruction of U. The return value is a cell
		% array UReconstr{i, j} indexed over spatial dimensions 
		% i=1,...,ndims and left/right value j=1,2 (only relevant for 
		% Cartesian meshes).
		UReconstr = reconstruct(obj, U, mesh)
	end
	
	
	methods
		%INITIALIZE
		% Run by runSolver()
		function initialize(obj, model)
			obj.model = model;
		end
	end
	
end