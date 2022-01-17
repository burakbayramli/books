function plotFrame( soln, U, t, zLim, fig, varname, varargin )
%PLOTFRAME   Plots a single frame
%
%   Arguments:
%   soln:     Solution object
%   U:        Scalar values to plot
%   t:        Time at which to plot
%   zLim:     Scaling of y-axis (1D) or z-axis (2D). zLim=0 lets MATLAB
%             decide the scaling.
%   fig:      Figure to plot in
%   varname:  Name of variable being plotted
%	varargin: (Optional) Any additional parameters will be sent to plot()
%			  when plotting 1D data.


	narginchk(4, 100);
	if nargin < 5
		fig = gcf();
	end
	if nargin < 6
		varname = '';
	end
	
	% If 'zLim' is a scalar, then let MATLAB decide the scaling of the
	% graph. If a vector, use zLim(1) and zLim(2) as min- and max-values.
	fixedAxis = true;
	if isscalar(zLim)
		fixedAxis = false;
	end	
	mesh = soln.mesh;
	set(0, 'CurrentFigure', fig);
	
	if isempty(varargin)
		varargin = { '-o' };
	end
	
	
	function doPlotFrame(u)
		% Ignore all but the first component
		u = u(1,:,:);
		
		% TODO: This is a quick fix
		if isa(mesh, 'Mesh.Tri')
			plotstyle = Plot.plotstyle();

	% 		% Plot ghost cell values
	% 		[border, ghosts, n] = soln.mesh.getGhostcells();
	% 		uGhost = u(:, ghosts);
	% 		xGhost = mesh.x(:, border);
	% 		u = u(:, mesh.internal);
	% 		plot3(xGhost(1,:), xGhost(2,:), uGhost, '.', 'linewidth', 2);
	% 		hold on;

			if strcmpi(plotstyle, 'surf')
				% Plot the solution as a 3D surface (on the dual of the mesh)
				trisurf(mesh.dual, mesh.x{1}, mesh.x{2}, u);
				if fixedAxis
					axis([mesh.xLim(1,:), mesh.xLim(2,:), zLim, zLim]);
				else
					xlim(mesh.xLim(1,:)); ylim(mesh.xLim(2,:));
				end

			elseif strcmpi(plotstyle, 'element')
				% Plot the solution as cellwise constants
				tri = mesh.t;
				x = mesh.p(:,1);
				y = mesh.p(:,2);
				trisurf(reshape(1:3*mesh.nx,3,mesh.nx)', x(tri'), y(tri'), [u;u;u]);
				if fixedAxis
					axis([mesh.xLim(1,:), mesh.xLim(2,:), zLim, zLim]);
				else
					xlim(mesh.xLim(1,:)); ylim(mesh.xLim(2,:));
				end

			elseif strcmpi(plotstyle, 'pcolor')
				% Pcolor plot

				% Plot on dual mesh
	% 			x = mesh.x(1,:);
	% 			y = mesh.x(2,:);
	% 			trisurf(mesh.dual, x, y, u);

				% Plot on the primary mesh
				tri = mesh.t;
				x = mesh.p(:,1);
				y = mesh.p(:,2);
				trisurf(reshape(1:3*mesh.nx,3,mesh.nx)', x(tri'), y(tri'), [u;u;u]);

				shading flat;
				set(gca(),'View',[0 90]);
				set(gca(),'Box','on');
				axis([mesh.xLim(1,:), mesh.xLim(2,:)]);
				set(gca, 'PlotBoxAspectRatio', [mesh.xRange(1), mesh.xRange(2), 1]);
				if fixedAxis
					caxis(zLim);
				end
			else
				error(['Plot style ''', plotstyle, ''' not implemented for unstructured meshes.']);
			end



		elseif mesh.ndims == 1
			% 1D data

			plot(mesh.x, u, varargin{:});
			if fixedAxis
				ylim(zLim);
			end
			xlabel('x');
			ylabel(varname);
			% Use equal scaling of each axis
	% 		set(gca, 'PlotBoxAspectRatio', [mesh.xRange(1), zLim(2)-zLim(1), 1]);


		else
			% 2D data

			X = squeeze(mesh.x{1});
			Y = squeeze(mesh.x{2});
			u = reshape(u, size(X));

			plotstyle = Plot.plotstyle();


			if strcmpi(plotstyle, 'pcolor')
				% Pcolor plot
	% 			pcolor(X, Y, u);
	% 			shading flat;

				% Use imagesc() instead of pcolor() to color cells, not vertices.
				x = mesh.xLim(1,:);
				y = mesh.xLim(2,:);
				if fixedAxis
					imagesc(x, y, u', zLim);
				else
					imagesc(x, y, u');
				end
				set(gca, 'YDir', 'normal');
				set(gca, 'PlotBoxAspectRatio', [mesh.xRange(1), mesh.xRange(2), 1]);


			elseif strcmpi(plotstyle, 'contour')
				% Contour plot
				contourf(X, Y, u);
				set(gca, 'PlotBoxAspectRatio', [mesh.xRange(1), mesh.xRange(2), 1]);


			elseif strcmpi(plotstyle, 'mesh')
				% Mesh plot
				meshc(X, Y, u);
				if fixedAxis
					zlim(zLim);
				end
				zlabel(varname);


			elseif strcmpi(plotstyle, 'surf')
				% Mesh plot
				surf(X, Y, u);
				if fixedAxis
					zlim(zLim);
				end
				zlabel(varname);


			else
				error('Unrecognized value of ''plotstyle''');
			end


			% Setup axes

			if fixedAxis
				caxis(zLim);
			end
			xlim(mesh.xLim(1,:));
			ylim(mesh.xLim(2,:));
	% 		axis equal;
	% 		colorbar;
			xlabel('x');
			ylabel('y');
		end
	end


	
	if iscell(U)
		for i = 1:length(U)
			doPlotFrame(U{i});
			hold on;
		end
		hold off;
		ulim = [min(min(U{1})), max(max(U{1}))];
	else
		doPlotFrame(U);
		ulim = [min(min(U)), max(max(U))];
	end	
	
	% Set title
	title(sprintf('t=%.2f\n(min, max) = (%.3e, %.3e)', ...
		t, ulim(1), ulim(2)));
end