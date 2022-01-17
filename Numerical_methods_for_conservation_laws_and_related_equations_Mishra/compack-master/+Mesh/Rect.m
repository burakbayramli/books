classdef Rect < Mesh.MeshBase
%RECT   Mesh class for rectangular 1D and 2D meshes.
%
%   See also the documentation for Mesh.MeshBase.
	
	
	properties
		internal	% Index into the internal of the grid (grid minus ghost 
					% cells). Usage:  u = U(dim, mesh.internal);
		xEdges		% Coordinates of cell edges
		intX		% x-component of Rect.internal
		intY		% y-component
		
		% "Left" and "right" values of cell interfaces in x- and y-direction
		leftX
		rightX
		leftY
		rightY
	end
	
	
	properties (Access = protected)
		% Index variables used in netFlux()
		intFluxX
		intFluxY
	end
	
	
	
	methods
		% Rect()
		% x and y are coordinates of cell edges. For 1D meshes, simply
		% don't pass the 'y' variable, or pass a scalar as 'y'.
		function o = Rect(x, y)
			narginchk(1, 2);
			
			if nargin == 1 || (nargin == 2 && length(y) == 1)
				o.ndims = 1;
			else
				o.ndims = 2;
			end
			
			x = sort(unique(x(:)))';
			o.nx = length(x)-1;
			xMid = (x(1:o.nx) + x(2:o.nx+1))/2;
			if o.ndims == 1
				o.xEdges = x;
				o.x = xMid;
				o.xLim = [min(x), max(x)];
				o.dx = { diff(x) };
				o.dxMin = min(o.dx{1});
			else	% o.ndims == 2
				y = sort(unique(y(:)))';
				o.xLim = [min(x), max(x); min(y), max(y)];
				o.nx = [length(x)-1, length(y)-1];
				yMid = (y(1:o.nx(2)) + y(2:o.nx(2)+1))/2;
				
				[X, Y] = meshgrid(x, y);
				o.xEdges = { X', Y' };
				
				% Compute grid sizes
				dx = { diff(x), diff(y) };
				[DX, DY] = meshgrid(dx{1}, dx{2});
				DX = DX'; DY = DY';
				o.dx = { reshape(DX, [1,size(DX)]), ...
					       reshape(DY, [1,size(DY)]) };
				o.dxMin = [min(dx{1}); min(dx{2})];
				
				[X, Y] = meshgrid(xMid, yMid);
				X = X'; Y = Y';
				o.x = { reshape(X, [1,size(X)]), ...
					    reshape(Y, [1,size(Y)]) };
			end
			o.xRange = o.xLim(:,2) - o.xLim(:,1);
			o.nxTot = prod(o.nx);
		end
		
		
		
		
		% refine()
		% Computes a refined mesh by splitting each cell in two (rectangle
		% in four for 2D meshes). The refinement is performed recursively n
		% times, and the new mesh has nx*2^n (nx*ny*4^n in 2D) grid cells.
		% The parameter n is optional; its default value is 1.
		function ret = refine(o, n)
			if nargin == 1
				n = 1;
			end
			assert(n >= 0, '''n'' must be a natural number');
			if n == 0
				ret = o;
				return;
			end
			
			if o.ndims == 1
				xNew = sort([o.xEdges; Utility.avg(o.xEdges)]);
				newMesh = Mesh.Rect(xNew);
			else
				xOld = o.xEdges{1}(:,1);
				yOld = o.xEdges{2}(1,:)';
				xNew = sort([xOld; Utility.avg(xOld)]);
				yNew = sort([yOld; Utility.avg(yOld)]);
				newMesh = Mesh.Rect(xNew, yNew);
			end
			ret = newMesh.refine(n-1);
		end
		
		
		
		% initialize()
		function initialize(o, config)
			% Initialize variables
			o.nelem = config.model.nelem;
			o.ngc = max(config.solver.ngc, config.reconstr.ngc);
			
			gsDiff = o.ngc - config.solver.ngc;
			o.intX = o.ngc+1 : o.ngc + o.nx(1);
			o.intFluxX = o.ngc + 1 : o.nx(1)+ o.ngc;  
            % ++++++ CHANGED FROM o.intFluxX = 2+gsDiff : o.nx(1)+2*o.ngc-gsDiff-1;
            % We assume that if a flux requires any ghost cells of its own,
            % it will need only the point values/ cell avg values for those
            % ghost cell. The reconstructed states will be only required
            % for the face being considered.
            
			o.leftX = 1+gsDiff : o.nx(1)+2*o.ngc-gsDiff-1;
			o.rightX = o.leftX + 1;
			o.intY = 1;
			o.intFluxY = 1;
			if o.ndims == 2
				o.intY = o.ngc+1 : o.ngc + o.nx(2);
				o.intFluxY = o.ngc + 1 : o.nx(2) + o.ngc;
               
				o.leftY = 1+gsDiff : o.nx(2)+2*o.ngc-gsDiff-1;
				o.rightY = o.leftY + 1;
			end

			% Build o.internal
			o.internal = false(o.nx + 2*o.ngc);
			o.internal(o.intX, o.intY) = true;
		end
		
		
		
		% netFlux()
		function ret = netFlux(o, numFlux, U, UReconstr, t, dt)
			ret = o.newMesh();
			
			Ul = UReconstr{1, 1};
			Ur = UReconstr{1, 2};
            flux = numFlux(1, Ur(:, o.leftX, o.intY), Ul(:, o.rightX, o.intY), ...
				t, dt, U(:, o.leftX, o.intY), U(:, o.rightX, o.intY));
			for k = 1:o.nelem
				ret(k, o.intFluxX, o.intY) = diff(flux(k,:,:), 1, 2) ./ o.dx{1}; % MUST ENSURE flux(k,:,l) has size o.intFluxX + 1
			end
			
			if o.ndims == 2
				Ul = UReconstr{2, 1};
				Ur = UReconstr{2, 2};				
				flux = numFlux(2, Ur(:, o.intX, o.leftY), Ul(:, o.intX, o.rightY), ...
					t, dt, U(:, o.intX, o.leftY), U(:, o.intX, o.rightY));
				for k = 1:o.nelem
					ret(k, o.intX, o.intFluxY) = ret(k, o.intX, o.intFluxY) + ...
						diff(flux(k,:,:), 1, 3) ./ o.dx{2};
				end
			end
		end
		
		
		
		% newMesh()
		function ret = newMesh(o)
			ret	= zeros([o.nelem, o.nx+2*o.ngc]);
		end
		
		
		
		% newMeshElem()
		function ret = newMeshElem(o, k)
			narginchk(1, 2);
			if nargin == 1
				k = 1;
			end
			ret = zeros([k, o.nx+2*o.ngc]);
		end
		
		
		
		% evalFunc()
		% See Mesh.MeshBase.evalFunc() for description.
		function ret = evalFunc(o, f, cellAvg)
			if o.ndims == 1
				% Find out how many components there are in the return values of f
				tmp = f(o.x(1));
				nElem = length(tmp);
				
				ret = o.newMeshElem(nElem);
				
				% Evaluate cell averages?
				if cellAvg
					% Integrate the function over each cell
					for i = 1:length(o.x)
						a = o.xEdges(i);
						b = o.xEdges(i+1);
						ret(:, o.intX(i)) = quadv(f, a, b);
					end
					% Divide by dx to get the average
					for k = 1:nElem
						ret(k, o.intX) = ret(k, o.intX) ./ o.dx{1};
					end
				else
					ret(:, o.intX) = f(o.x);
				end
			else				
				% Find out how many components there are in the return values of f
				tmp = f(o.x{1}(1), o.x{2}(1));
				nElem = length(tmp);
				
				ret = o.newMeshElem(nElem);
				
				% Integrating numerically over a 2D domain is very
				% costly, so just use point-wise evaluation for the
				% moment.
				x = o.x{1};
				y = o.x{2};
				ret(:, o.intX, o.intY) = f(x, y);
			end
		end
	end
end