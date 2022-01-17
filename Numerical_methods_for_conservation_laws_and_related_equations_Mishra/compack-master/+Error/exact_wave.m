function u = exact_wave( initial )
%EXACT_WAVE   Computes the exact solution of the wave equation.
%   Returns a function handle to a function that computes the exact 
%   solution of the wave equation with the given initial data.
%
%   Arguments:
%   initial:    Function pointer returning the initial data.
%
%   See also the wave equation model class Model.Wave.
	


	function ret = exact(mesh, t, cellAvg)
		uBack = mesh.evalFunc(@(y) initial(y-t), cellAvg);
		uForw = mesh.evalFunc(@(y) initial(y+t), cellAvg);
		w1 = 0.5*(uBack(1,:,:) + uBack(2,:,:));
		w2 = 0.5*(uForw(1,:,:) - uForw(2,:,:));
		ret = [w1 + w2; w1 - w2; zeros(size(w1))];
	end


	u = @exact;


end

