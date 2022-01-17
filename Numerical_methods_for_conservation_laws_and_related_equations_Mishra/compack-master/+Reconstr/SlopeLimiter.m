classdef SlopeLimiter < Reconstr.Reconstruct
	
	properties (SetAccess = protected)
		ngc = 2
	end
	
	
	properties(SetAccess = private)
		limiter
	end
	
	
		
	methods
		%SLOPELIMITER
		% The parameter 'limiter' is an object of any class that implements
		% a method 
		%	slope(ul, um, ur)
		% which returns a slope, multiplied by dx.
		function obj = SlopeLimiter(limiter)
			obj.limiter = limiter;
		end		
		
		
		function UReconstr = reconstruct(obj, U, mesh)
			% Reconstruct component-wise in x-direction
			Ul = mesh.newMesh();
			Ur = mesh.newMesh();
			for i = 1:mesh.nelem
% 				u = squeeze(U(i,:,:));
				u = shiftdim(U(i,:,:), 1);	% Move the 2nd and 3rd dimensions of U to dimensions 1 and 2
				s = obj.slopes(u, 1);
				Ul(i,:,:) = u - s/2;
				Ur(i,:,:) = u + s/2;
			end
			
			if mesh.ndims == 1
				UReconstr = { Ul, Ur };
			else
				% If computing in two dimensions, reconstruct in
				% y-direction, too.
				Ub = mesh.newMesh();
				Ut = mesh.newMesh();
				for i = 1:mesh.nelem
					u = squeeze(U(i,:,:));
					s = obj.slopes(u, 2);
					Ub(i,:,:) = u - s/2;
					Ut(i,:,:) = u + s/2;
				end
				UReconstr = { Ul, Ur; Ub, Ut };
			end
		end
	end
	
	
	methods(Access = private)
		% Computes slopes with the limiter in direction d.
		function s = slopes(obj, u, d)
			if d == 2
				% If reconstructing in y-direction, flip the dimensions,
				% reconstruct in x-direction and flip back again.
				u = u';
			end
			s = zeros(size(u));
			i = 2 : size(u,1)-1;
			s(i,:) = obj.limiter.slope(u(i-1,:), u(i,:), u(i+1,:));
			if d == 2
				s = s';
			end
		end
	end
end

