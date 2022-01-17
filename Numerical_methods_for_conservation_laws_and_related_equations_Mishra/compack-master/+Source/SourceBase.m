classdef SourceBase < handle
%SOURCEBASE Base class for source terms
%   Classes deriving from SourceBase must implement a function source() that
%   computes the source term contribution to each cell.
	
	methods
		% initialize()
		% This function is called by runSolver() before the simulation starts.
		% Default behavior is to do nothing.
		function initialize(o, config)
		end
	end	
	
	
	methods (Abstract)
		% source()
		% Compute the source term contribution in each grid cell. The return
		% value must be of size size(U).
		ret = source(o, U, UR, t, dt)
	end
end