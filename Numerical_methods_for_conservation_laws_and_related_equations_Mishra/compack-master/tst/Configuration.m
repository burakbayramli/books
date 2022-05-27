classdef Configuration < handle
	%CONFIGURATION   
	
	
	properties
		model
		source
		mesh
		
		% If true, consider the unknown as a cell average; if false,
		% consider it as a point wise value. In particular, in the former
		% case the initial data is sampled as cell averages, whereas in the
		% latter case it is evaluated point wise.
		useCellAvg = true
		
		% Boundary condition object
		bc
		
		% Spatial reconstruction routine. These are found in the
		% Reconstr package.		
		reconstr = Reconstr.NoReconstr
		
		% Time integration routine. These are found in the TimeIntegration
		% package.		
		timeInt = @TimeIntegration.FE
		
		% Compute up to time t=tMax
		tMax
		
		% If the user wants specific times to be included in the
		% simulation, put those here. Elements must satisfy
		%	0 < tInclude(i) <= tMax,
		% or else an error will be raised. Note that the solution at t=0
		% and t=tMax will always be included.
		tInclude = []
		
		% CFL number
		CFL = 0.5
		
		% Maximum number of time steps to write to the Solution object
		maxNumWrite = 100
		
		% Type of solver to use. These are found in the Flux package.
		solver
		
		% Initial data. Must be a function taking a vector x as input and
		% returns the initial data at those points. For 2D problems, the
		% function must take two arguments (x,y).
		initial
		
		% If false, no output will be written to console by runSolver()
		verbose = true
	end
	
	
	
	methods
		% Configuration()
		% Copy constructor
		function ret = Configuration(rhs)
			if nargin == 1
				assert(isa(rhs, 'Configuration'), 'Copy constructor parameter must be of type Configuration');
				prop = properties(rhs);
				for i=1:length(prop)
					ret.(prop{i}) = rhs.(prop{i});
				end
			end
		end
		
		
		
		% processInput()
		% Verify user input
		function processInput(obj)
			% Check that neccessary variables have been set
			obj.checkVar('model');
			obj.checkVar('mesh');
			obj.checkVar('bc');
			obj.checkVar('tMax');
			assert(isreal(obj.tMax) && obj.tMax > 0, ...
				'The Configuration parameter ''tMax'' must be a positive number');
			obj.checkVar('solver');
			obj.checkVar('initial');
			assert(isreal(obj.maxNumWrite) && isfinite(obj.maxNumWrite) ...
				&& obj.maxNumWrite >= 2, ...
				'The Configuration parameter ''maxNumWrite'' must be an integer >= 2.');
			if ~isempty(obj.tInclude)
				assert(all(isreal(obj.tInclude) & (obj.tInclude >= 0) & (obj.tInclude <= obj.tMax)), ...
					'The Configuration parameter ''tInclude'' must consist of numbers 0<=t<=tMax');
			end
		end
	end
	
	
	
	methods (Access = private)
		% checkVar()
		% Checks that the variable member with name 'varName' is present
		% in the object, and if not, raises an error.
		function checkVar(obj, varName)			
			var = eval(['obj.' varName]);
			assert(~isempty(var), ['Configuration variable "conf.' varName '" not present']);
		end
	end
	
end