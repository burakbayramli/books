function u = exact_linEuler( initial )
%EXACT_LINEULER   Computes the exact solution of the 1D linearized Euler equation.
%   Returns a function handle to a function that computes the exact 
%   solution of the wave equation with the given initial data.
%
%   Arguments:
%   initial:    Function pointer returning the initial data.
	


	model = Model.LinEuler();
	[R, RInv] = model.eigenvectors([], 1);
	Gamma = model.eigenvalues([], 1);
	function ret = exact(mesh, t, cellAvg)
		W = mesh.newMesh();
		% x is the translated mesh in the p-th wave family
		x = mesh.newMesh();
		
		for p=1:size(W, 1)
			x(p, :) = mesh.x - Gamma(p,p)*t;
		end
		
		for j=1:length(W(1,:))
			W(:, j) = RInv*
		end
	end


	u = @exact;


end

