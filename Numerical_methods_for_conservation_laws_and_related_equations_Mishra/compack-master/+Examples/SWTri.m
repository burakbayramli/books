function soln = SWTri()
	conf = Configuration();
	conf.model = Model.SW;
	conf.model.grav = 1;
	conf.timeInt = @TimeIntegration.RK2;
	conf.tMax = 20;
	conf.CFL = 0.4;
	conf.maxNumWrite = 100;
	
	
	nx = 100;
	if 0
		mesh = Mesh.Tri.Cartesian([-50, 50; -50, 50], [nx, nx], 'right');

% 		p = 50*[2, 1; -2, 1; 0, 1-sqrt(12)];
% 		t = [1, 2, 3];
% 		mesh = Mesh.Tri(p,t);
% 		mesh = mesh.refine(7);
		
	
% 		conf.solver = Flux.Tri.Rusanov;
		conf.solver = Flux.Tri.SW.EC;
% 		conf.solver = Flux.Tri.SW.ERus;

		conf.bc = Mesh.BC.Tri.Neumann;
% 		conf.bc = Mesh.BC.Tri.SWReflect;
		conf.mesh = mesh;
	else
% 		conf.solver = Flux.Rusanov;
		conf.solver = Flux.SW.EC;

		conf.mesh = Mesh.Cartesian([-50,50; -50,50], [nx,nx]);
		conf.bc = Mesh.BC.Neumann;
% 		conf.bc = Mesh.BC.Periodic;
	end
	
	
	
	function U0 = initial(x, y)
		% Default parameter values
% 		a = pi/6;
% 		x0 = -20;
% 		y0 = -10;
		a = 0;
		x0 = 0;
		y0 = 0;
		M = 0.5;
% 		M = 0;
		c1 = -0.04;
		c2 = 0.02;
		grav = conf.model.grav;
		
		ef = exp(-c2*((x-x0).^2 + (y-y0).^2));
		h = 1 - c1*c1/(4*c2*grav)*ef.*ef;
		hu = h .* (M*cos(a) + c1*(y-y0).*ef);
		hv = h .* (M*sin(a) - c1*(x-x0).*ef);
		U0 = [h; hu; hv];
	end

% 	function U0 = initial(x,y)
% 		r = sqrt(x.^2 + y.^2);
% 		h = 1+(r<20);
% % 		h = 1 + 0.2*sin(2*pi*x/50);
% % 		h = ones(size(x));
% 		q = zeros(size(x));
% 		U0 = [h; q; q];
% 	end


	conf.initial = @initial;

	
	
	soln = runSolver(conf);
	
% 	ulim = 0;
% 	ulim = [0.8, 1.5];
% 	ulim = [1,2];
	ulim = [0.98, 1];
	Plot.plotSolution(soln, ulim, 'h');	
	
	
	
	% Compute error w.r.t. initial data
	[~, U0] = soln.get(1);
	[~, U1] = soln.get(soln.end);
	u0 = soln.getVariable(U0, 1);
	u1 = soln.getVariable(U1, 1);
	fprintf('error=%e\n', Error.calcNorm(soln.mesh, u1-u0, 1));
end
