classdef ENO < Reconstr.Reconstruct
	%ENO   Essentially nonoscillatory reconstruction.
	
	
	properties (SetAccess=protected)
		ngc
		k
	end
	
	properties (Access = protected)
		Crj
	end
	
	
	
	methods
		% ENO()
		% ENO constructor. Takes as argument k, the accuracy of the
		% reconstruction. An optional argument specifies the path of the
		% ENO coefficients.
		function obj = ENO(k, varargin)
			obj.ngc = k;
			% Load ENO coefficients of u_j for a uniform mesh
			if isempty(varargin)
				coeffPath = '+Reconstr/ENO_coeff';
			else
				coeffPath = varargin{1};
			end
			S = load(coeffPath);
			obj.Crj = S.Crj{k}';
			obj.k = k;
		end
		
		
		
		% reconstruct()
		% 1D ENO reconstruction
		function UReconstr = reconstruct(obj, U, mesh)
			if mesh.ndims == 1
				Ul = mesh.newMesh();
				Ur = mesh.newMesh();
				for d = 1:mesh.nelem
					[Ul(d,:), Ur(d,:)] = obj.doReconstr(U(d, :), mesh);
				end
				UReconstr = { Ul, Ur };
			else
				error('Reconstruction in 2D and 3D not implemented');
			end
		end
	end
	
	
	
	methods(Access = protected)
		% doReconstr()
		% Performs the actual ENO reconstruction on a vector "u"
		function [ul, ur] = doReconstr(obj, u, mesh)
			% Initialize
			
			% Total number of cells
			n = length(u);

			
			% Compute undivided difference of the primitive of u
			
			dd = zeros(obj.k, n);
			dd(1,:) = u;
			for j = 2:obj.k
				dd(j, 1:n+1-j) = dd(j-1, 2:n+2-j) - dd(j-1, 1:n+1-j);
			end

			
			% Determine r, the left-displacement of index
			
			r = ones(1, n);
			% Indices to each stencil
			stencils = ones(obj.k, n);
			for m = obj.k : n-obj.k+1
				curR = 0;
				for l = 2:obj.k
					if abs(dd(l, m-curR-1)) <= abs(dd(l, m-curR))
						curR = curR+1;
					end
				end
				stencils(:, m) = m-curR : m-curR + obj.k-1;
				r(m) = curR;
			end
			
			
			% Compute left and right reconstructed values
			[ul, ur] = obj.interpolate(u, r, stencils);
		end
		
		
		
		% interpolate()
		function [ul, ur] = interpolate(obj, u, r, stencils)
			ul = dot(obj.Crj(:, r+1), u(stencils));
			ur = dot(obj.Crj(:, r+2), u(stencils));
		end
	end
	
end

