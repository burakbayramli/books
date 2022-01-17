function u = exact_linAdv( initial )
%EXACT_LINADV   Computes the exact solution of the transport equation.
%   Returns a function handle to a function that computes the exact 
%   solution of the linear advection problem with the given initial data.
%
%   Arguments:
%   initial:    Function pointer returning the initial data.



	function ret = exact(mesh, t, varargin)
		exactPointwise = @(x) initial(x-t);
		ret = mesh.evalFunc(exactPointwise, varargin{:});
	end


	u = @exact;


end

