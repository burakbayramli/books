classdef Tri < Mesh.MeshBase
%TRI   Mesh class for unstructured triangular 2D meshes.
%   See also the documentation for Mesh.MeshBase.
	

	% From Mesh.MeshBase:
	properties
		internal
% 		x		% Cell array of size 2 with x- and y-coordinates in each of the 
% 				% components.
% 		nx		% Number of gridcells (scalar value)
%		ngc		% The _total_ number of ghost cells in the mesh. Only a
				% ghost cell "depth" of 1 is supported -- that is, cells at
				% the border may only have one neighboring ghost cell, and
				% ghost cells themselves do not have neighboring ghost
				% cells.
	end
	
	properties
		ne		% Number of edges in the mesh
		np		% Number of vertices
		
		p		% np*2 array of vertices
		t		% nx*3 array describing the triangulation. t(i,:) are 
				% indices (into 'p') of the vertices of triangle i.
		edges
		atEdge	% Index of all edges that lie on the border of the domain.
		c2e		% nx*3 array mapping cell indices to edge indices. c2e(j,i) 
				% is the index (into 'edges') of the edge between cells i 
				% and nbs(i,j).
		left	% ne*1 array of indices of the cell "to the left" of each 
				% edge.
		right
		normals % ne*2 array of edge normals pointing from "left" into "right".
		dS		% nx*3 array of reals. dS(i,j) equals:
				%	 ds(i,j)/dA(i)	if cell i is to the left of nbs(i,j)
				%	-ds(i,j)/dA(i)	if cell i is to the right of nbs(i,j)
				% where ds(i,j) is the length of the edge c2e(i,j) and
				% dA(i) is the area of cell i.
		dual
	end
	
	
	
	methods (Static)
		% CARTESIAN   Create a triangulated cartesian mesh.
% 		%    mesh = Mesh.Tri.Cartesian(xLim, nc) creates a Cartesian mesh with
		%    boundaries xLim and number of grid cells nc, and then triangulates
		%    the mesh. The optional parameter 'orientation' can be either of the
		%    strings 'left', 'right' and 'random', and determines the
		%    orientation of each triangle. The default value is 'left'.
		%
		%    Examples:
		%       mesh = Mesh.Tri.Cartesian([0,1;0,1], [100,100]); discretizes the 
		%          domain [0,1]x[0,1] into 100x100 rectangular grid cells, which
		%          are then split in two from lower-left to top-right.
		%       mesh = Mesh.Tri.Cartesian([0,1;0,1], [100,100], 'right'); 
		%          discretizes the domain [0,1]x[0,1] into 100x100 grid cells,
		%          which are split into two from upper-left to lower-right.
		function o = Cartesian(xLim, nc, orientation)
			% Check input data
			error(nargchk(2, 3, nargin));
			assert(ndims(xLim)==2 && all(size(xLim)==[2,2]), 'Parameter ''xLim'' must be a 2x2-array.');
			assert(xLim(1,1) < xLim(1,2) && xLim(2,1) < xLim(2,2), 'Lower boundary limit must be smaller than upper limit.');
			assert(length(nc)==2, 'Parameter ''nc'' must be a 2-vector.');
			if nargin == 3
				assert(ischar(orientation), 'Parameter ''orientation'' must be a string.');
				assert(strcmpi(orientation, 'left') || strcmpi(orientation, 'right') || strcmpi(orientation, 'random'), ...
					'Parameter ''orientation'' must have either of the values ''left'', ''right'' or ''random''.');
			else
				orientation = 'left';
			end
			
			% Create a rectangular mesh
			nx = nc(1)+1;
			ny = nc(2)+1;
			x = linspace(xLim(1,1), xLim(1,2), nx);
			y = linspace(xLim(2,1), xLim(2,2), ny);
			[X, Y] = meshgrid(x, y);
			X = X(:); Y = Y(:);
			
			if strcmpi(orientation, 'random')
				% If orientation is 'random' then just use Delaunay triangulation
				tri = delaunay(X, Y);
			else
				% Otherwise, create a mesh where all triangles are oriented the
				% same way.
				tri = [];
				i = (1:(nx-1))';
				if strcmpi(orientation, 'left')
					for j = 0:nx:nx*(ny-2)
						tri = [tri; j+i, j+i+1, j+i+nx+1; ...
							j+i, j+i+nx+1, j+i+nx];
					end
				else
					for j = 0:nx:nx*(ny-2)
						tri = [tri; j+i, j+i+1, j+i+nx; ...
							j+i+1, j+i+nx+1, j+i+nx];
					end
				end
			end
			o = Mesh.Tri([X,Y], tri);
		end
	end
	
	
	
	methods
		% Tri()
		% 
		% Input:
		% p:	N*2 array of vertex coordinates
		% t:	M*3 array of triangles
		function o = Tri(p, t)
			error(nargchk(2, 2, nargin));
			assert(size(p,2) == 2, 'Parameter ''p'' must be an array of size numVertices*2');
			assert(size(t,2) == 3, 'Parameter ''t'' must be an array of size numTriangles*3');
			
			% Ensure that the triangle is oriented counter-clockwise
			t = o.ensureCC(p, t);

			o.ndims = 2;
			o.p = p;
			o.np = size(p,1);
			o.t = t;
			x = p(:,1);
			y = p(:,2);
			o.x = { sum(x(t),2)'/3, sum(y(t),2)'/3 };
			o.nx = size(o.t, 1);
			o.nxTot = o.nx;
			o.xLim = [min(x), max(x); min(y), max(y)];
			o.xRange = diff(o.xLim, 1, 2);
			o.internal = 1 : o.nx;
			
			TR = TriRep(t, p(:,1), p(:,2));
			edges = TR.edges;
			o.ne = length(edges);
						
			e2c = TR.edgeAttachments(edges);	% Edge-to-cell indices
			e2cLength = cellfun(@length, e2c');
			o.atEdge = find(e2cLength == 1);
			o.ngc = length(o.atEdge);
			
			
			%%% Assemble o.left
			% Extract the first component of every element of the cell array e2c
% 			lc = cellfun(@(x)x(1), e2c');		% This is about 7 times slower
			e2cIndex = [1, 1+cumsum(e2cLength)];
			e2cIndex = e2cIndex(1 : o.ne);
			e2cMat = cell2mat(e2c');
			lc = e2cMat(e2cIndex);
			o.left = lc;
			
			
			% Make sure that the cell to the "left" of each edge sees that edge
			% as being oriented counter-clockwise.
			reverseIndex = any(edges ~= t(lc,[1,2]),2) ...
				& any(edges ~= t(lc,[2,3]),2) ...
				& any(edges ~= t(lc,[3,1]),2);
			edges(reverseIndex,[1,2]) = edges(reverseIndex,[2,1]);
% 			edges(:,[1,2]) = edges(:,[2,1]);
			o.edges = edges;
			
			
			%%% Build the right and c2e arrays by looping through all edges.
			o.right = zeros(1, o.ne);
			o.right(o.atEdge) = o.nx+1 : o.nx+o.ngc;	% Ghost cells
			c2e = zeros(o.nx, 3);
			for k = 1:o.ne
				lcCur = lc(k);		% Left cell
				
				% Insert the current edge into c2e in the position corresponding
				% to the position of the first vertex of the edge in the
				% triangle 'lc'.
				index = t(lcCur,:) == edges(k,1);
% 				if c2e(lcCur, index)
% 				end
				c2e(lcCur, index) = k;
				% Is edge k in the interior?
				if length(e2c{k}) > 1
					% Add the right cell to c2e
					rc = e2c{k}(2);
					o.right(k) = rc;
					index = t(rc,:) == edges(k,2);
% 					if c2e(rc, index)
% 					end
					c2e(rc, index) = k;
				end
			end
			o.c2e = c2e;
			

			% Side lengths
			de = sqrt(sum((p(edges(:,2),:) - p(edges(:,1),:)).^2, 2));
			% Semiperimeter
			s = sum(reshape(de(o.c2e),size(o.c2e)), 2)/2;	% The reshape() call is necessary in the special case nx=1
			% Set dx as the cell area
			inradius = sqrt((s-de(o.c2e(:,1))) .* (s-de(o.c2e(:,2))) .* (s-de(o.c2e(:,3))) ./ s);
			o.dxMin = 2*min(inradius)*[1; 1];	%TODO: Find the correct CFL condition. Mpy radius by 2?
			area = inradius.*s;
			o.dx = area';

			
			%%% List of normals of each edge
			edge = p(edges(:,2),:) - p(edges(:,1),:);
			edge = [edge(:,2), -edge(:,1)]';
			o.normals = Utility.divideArray(edge, sqrt(sum(edge.^2)));
			
			
% 			dS = zeros(o.nx, 3);
% 			for i = 1:o.nx
% 				edge = p(t(i,[2,3,1]),:) - p(t(i,[1,2,3]),:);
% 				edge = [-edge(:,2),edge(:,1)];
% 				dS(i,:) = dot(edge, ...
% 					normals(:,c2e(i,:))', ...
% 					2) / area(i);
% 			end

			% The following is a vectorization of the above.
			edge = (p(t(:,[2,3,1]),:) - p(t(:,[1,2,3]),:))';
			edge = [edge(2,:); -edge(1,:)];
			o.dS = reshape(dot(edge, o.normals(:,o.c2e)), [o.nx, 3]);
			o.dS = Utility.divideArray(o.dS', area')';

						
% 			for k = 1:o.ne
% 				if min(abs(o.normals(:,k))) > 1e-10
% 					ind = o.c2e(o.left(k), :) == k;
% 					assert(dS(o.left(k),ind) > 1.4);
% 					ind = o.c2e(o.right(k), :) == k;
% 					assert(dS(o.right(k),ind) < -1.4);
% 				end
% 			end
			
			% Check dS
% 			atEdgeInd = false(o.ne,1);	atEdgeInd(o.atEdge) = true;
% 			hasChecked = false(size(o.dS));
% 			for k = 1:o.ne
% 				assert(o.dS(o.left(k),o.c2e(o.left(k),:) == k) > 0);
% % 				assert(~hasChecked(o.left(k),o.c2e(o.left(k),:) == k));
% % 				hasChecked(o.left(k),o.c2e(o.left(k),:) == k) = true;
% 				if atEdgeInd(k)
% 					disp(o.dS(o.left(k),:));
% 				else
% 					assert(o.dS(o.right(k),o.c2e(o.right(k),:) == k) < 0);
% % 					assert(~hasChecked(o.right(k),o.c2e(o.right(k),:) == k));
% % 					hasChecked(o.right(k),o.c2e(o.right(k),:) == k) = true;
% 				end
% 			end

% 			atEdgeInd = false(o.ne,1);	atEdgeInd(o.atEdge) = true;
% 			for k = 1:o.ne
% 				if atEdgeInd(k)
% 					
% % 					display(k);
% % 					disp(p(edges(k,:),:));
% % 					disp(o.normals(:,k));
% % 					cell = e2c{k};
% % 					index = o.c2e(cell,:)==k;
% % 					assert(abs(o.dS(cell,index)-80)<1e-10);
% 				else
% 					c1 = o.left(k); c2 = o.right(k);
% 					ind1 = o.c2e(c1,:) == k;
% 					ind2 = o.c2e(c2,:) == k;
% 					assert(o.dS(c1,ind1) == -o.dS(c2,ind2) && o.dS(c1,ind1)>0);
% 				end
% 			end
			
			
			%%% Build the "dual" of the mesh
			% 'dual' is used in plotting, and gives a triangulation of the
			% vertex-centered dual of the mesh.
			vertexAtt = TR.vertexAttachments();
			% Extract vertices attached to more than 2 cells
			v2c = vertexAtt(cellfun(@length, vertexAtt) > 2);
			% nv is the number of triangles that make up the dual
			nv = sum(cellfun(@length, vertexAtt)-2);
			dual = zeros(nv, 3);
			i = 1;
			for j = 1:length(v2c)
				for k = 3:length(v2c{j})
					% Attach the first triangle to subsequent triangles in
					% sequence.
					dual(i,:) = v2c{j}([1, k-1, k]);
					i = i+1;
				end
			end
			o.dual = dual;
		end
		
		
		
		% refine()
		% Computes a refined mesh by splitting each edge in half and
		% retriangulating. The new mesh has nx*4^n number of gridcells. The
		% refinement for n>1 is done recursively.
		% The parameter n is optional; its default value is 1.
		function ret = refine(o, n)
			error(nargchk(1, 2, nargin));
			if nargin == 1
				n = 1;
			end
			assert(n >= 0, '''n'' must be a natural number');
			if n == 0
				ret = o;
				return;
			end
			
			% Compute edge midpoints
			midp = (o.p(o.edges(:,1),:) + o.p(o.edges(:,2),:))/2;
			% Add midpoints to the list of vertices
			pNew = [o.p; midp];
			% Retriangulate
			tNew = zeros(4*o.nx, 3);
			for i = 1:o.nx
				eCur = o.c2e(i,:) + o.np;	% Index (into midp) of the current edge endpoints
				tNew(4*i-3,:) = eCur;		% Center triangle
				tNew(4*i-2,:) = [o.t(i,1), eCur(1), eCur(3)];
				tNew(4*i-1,:) = [eCur(1), o.t(i,2), eCur(2)];
				tNew(4*i,:)   = [eCur(2), o.t(i,3), eCur(3)];
			end

			% Compute new Mesh object and refine recursively
			meshNew = Mesh.Tri(pNew, tNew);
			ret = meshNew.refine(n-1);
		end
		
		
		
		% initialize()
		function initialize(o, config)
			o.nelem = config.model.nelem;
		end
		
		
		
		% netFlux()
		% TODO: Implement reconstruction
		function ret = netFlux(o, numFlux, U, UReconstr, t, dt)
			ret = o.newMesh();
			flux = numFlux(o.normals, U(:,o.left), U(:,o.right), t, dt);
			for k = 1:o.nelem
				ret(k,o.internal) = dot(reshape(flux(k,o.c2e),size(o.c2e)), o.dS, 2);
			end
			
			atEdgeInd = false(o.ne,1);	atEdgeInd(o.atEdge) = true;
			atEdgeInd = atEdgeInd & ((o.p(o.edges(:,1),1)==-1) & o.p(o.edges(:,2),1)==-1);
		end
		
		
		
		% getGhostcells()
		% Returns the indices of ghost cells ('ghosts') and the
		% corresponding grid cell at the boundary ('border'), along with
		% the normals of the edges between them.
		function [border, ghosts, normals] = getGhostcells(o)
			border = o.left(o.atEdge);
			ghosts = o.right(o.atEdge);		%o.nx+1 : o.nx+o.ngc;
			normals = o.normals(:,o.atEdge);
		end
		
		
		
		% newMesh()
		function ret = newMesh(o)
			ret = zeros(o.nelem, o.nx+o.ngc);
		end
		
		
		
		% newMeshElem()
		function ret = newMeshElem(o)
			ret = zeros(1, o.nx+o.ngc);
		end
		
		
		
		% evalFunc()
		% See Mesh.MeshBase.evalFunc() for description.
		function ret = evalFunc(o, func, cellAvg)
			ret = o.newMesh();
			if cellAvg
				% Evaluate cell averages (NOT IMPLEMENTED)
				ret = o.evalFunc(func, false);
			else
				% Evaluate point-wise
				ret(:, o.internal) = func(o.x{1}, o.x{2});				
			end
			
% 			u = ret(1, o.internal);
% 			plot3(x, y, u, '.');
			
% 			x = mesh.p(:,1);
% 			y = mesh.p(:,2);
% 			trisurf(reshape(1:3*o.nx,3,o.nx)', x(tri'), y(tri'), [u;u;u]);
		end
	end
	
	
	
	methods (Access=private)
		% ensureCC()
		% Makes sure all triangles are oriented counter-clockwise.
		function t = ensureCC(o, p, t)
			% Assume that the triangulation is already clockwise
			p1 = t(:,1);
			p2 = t(:,2);
			t(:,1) = p2;
			t(:,2) = p1;
			
			nx = size(t,1);
			edge1 = p(t(:,2),:) - p(t(:,1),:);
			edge2 = p(t(:,3),:) - p(t(:,2),:);
			tmp = cross([edge1,zeros(nx,1)], [edge2,zeros(nx,1)]);
			% Get the index of all triangles with the wrong orientation
			reverseIndex = tmp(:,3) < 0;
			% Switch the first and second vertices of those triangles
			p1 = t(:,1);
			p2 = t(:,2);
			t(reverseIndex,1) = p2(reverseIndex);
			t(reverseIndex,2) = p1(reverseIndex);
		end
	end
end