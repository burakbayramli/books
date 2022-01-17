classdef SlopeLimiterEigen < Reconstr.Reconstruct
	%SLOPELIMITEREIGEN   Reconstruction in eigenspace.
	
	properties (SetAccess = protected)
		ngc = 2
	end
	
	
	properties(SetAccess = private)
		limiter
	end
	
	
		
	methods
		%SLOPELIMITEREIGEN
		% The parameter 'limiter' is an object of any class that implements
		% a method 
		%	slope(ul, um, ur)
		% which returns a slope, multiplied by dx.
		function obj = SlopeLimiterEigen(limiter)
			obj.limiter = limiter;
		end
		
		
		function UReconstr = reconstruct(obj, U, mesh)
			if mesh.ndims == 1
				% Transform U into eigenspace coordinates W
				W = mesh.newMesh();
				m = size(U, 1);
				numX = size(U, 2);
				R = zeros(m, m, numX);
				for j = 1:numX
					[R(:,:,j), RInv] = obj.model.eigenvectors(U(:,j), 1);
					W(:, j) = RInv*U(:,j);
				end
				
				% Reconstruct in eigenspace
				Wl = mesh.newMesh();
				Wr = mesh.newMesh();
				% Reconstruct W component-wise
				for i = 1:mesh.nelem
					w = W(i, :);
					s = obj.slopes(w);
					Wl(i, :) = w - s/2;
					Wr(i, :) = w + s/2;
				end
				
				% Transform back to conserved variables
				Ul = mesh.newMesh();
				Ur = mesh.newMesh();
				for j = 1:numX
					Ul(:, j) = R(:,:,j)*Wl(:,j);
					Ur(:, j) = R(:,:,j)*Wr(:,j);
				end				
				UReconstr = { Ul, Ur };
			else
				error('Reconstruction in 2D and 3D not implemented');
			end
		end
	end
	
	
	methods(Access = private)
		function s = slopes(obj, u)
			s = zeros(size(u));
			i = 2 : length(u)-1;
			s(i) = obj.limiter.slope(u(i-1), u(i), u(i+1));
		end
	end
end

