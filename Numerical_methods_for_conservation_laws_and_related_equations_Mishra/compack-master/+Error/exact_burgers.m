function u = exact_burgers( initial )
%EXACT_BURGERS   Computes the exact solution of Burgers' equation.
%   Given the initial data, the function returns a function pointer to a
%   method that computes the exact solution of the 1D Burgers equation on a
%   given mesh. The initial data must be smooth, and after the appearance
%   of shocks the solution returned by the function may be considered
%   invalid.
%
%   Arguments:
%   initial:    Function pointer returning the initial data.


	function ret = exact(mesh, t, cellAvg)
		function z = exactPointwise(y)
			z = zeros(1, length(y));
			for i = 1:length(y)
				f = @(a) y(i) - initial(a)*t - a;
				x0 = fzero(f, y(i));
				z(i) = initial(x0);
			end
		end
		ret = mesh.evalFunc(@exactPointwise, cellAvg);
	end


	u = @exact;


end