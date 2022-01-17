classdef Solution < handle
	%SOLUTION   Contains all information related to the numerical solution.
	% The output from runSolver() is a Solution object. This object
	% contains the computed solution at all points of time, along with
	% metadata such as the initial data function, the computational domain,
	% etc.
	
	
	properties(Access=private, Hidden)
		solnMapQueue	% Solution queue
		numQueue = 0	% Number of solutions in the queue
		tInclude		% Timesteps that should be included
	end
	
	properties (SetAccess = private)
		config
		mesh
		initial
		modelName
		solnMap			% A cell array containing all computed solutions
		numSoln 		% Number of entries in the solnMap object
	end
	
	properties
		timeElapsed		    % Total time used by the solver (set by runSolver())
							% TODO: Rename this to "runtime"
		maxMemUsage = 256	% Maximum amount of memory (in MB) to use. This 
							% is only an indicative number; changing it
							% does not guarantee a certain memory usage
							% limit.
	end
	
	
	
	
	methods
		% Solution()
		function obj = Solution(config)
			obj.config = config;
			obj.mesh = config.mesh;
			obj.initial = config.initial;
			obj.solnMap = obj.newSolnMap();
			obj.solnMapQueue = obj.newSolnMap();
			obj.numSoln = 0;
			obj.modelName = config.model.name;
			
			tLin = linspace(0, obj.config.tMax, obj.config.maxNumWrite);
			% Make sure elements of config.tInclude are included
			obj.tInclude = sort(unique([tLin, obj.config.tInclude]));
		end


		% getVariable()
		% Extracts the component with name 'varname' from U. This function 
		% calls the corresponding function in config.model.
		%
		% Input: 
		% U:		Solution at a specific point in time, obtained from
		%			getAtTime()	or getFinal().
		% varname:	(Optional) Name of variable to return. Default is varname=1,
		%			the first component.
		function ret = getVariable(soln, U, varname)
			ret = soln.config.model.getVariable(soln, U, varname);
		end
		
		
		% readInitialData()
		% Reads and returns the initial data
		function U0 = readInitialData(obj)
			% Read initial data
			U0 = obj.mesh.evalFunc(obj.initial, obj.config.useCellAvg);
		end
		
		
		% pushSolution()
		% Pushes a solution to the solution stack.
		function pushSolution(obj, U, t)
			% Push the solution to the queue
			obj.numQueue = obj.numQueue + 1;
			obj.solnMapQueue{obj.numQueue, 1} = t;
			obj.solnMapQueue{obj.numQueue, 2} = U;
						
			% Should the queue be flushed?
			if obj.memUsage() > obj.maxMemUsage
				obj.flushQueue();
			end
		end
		
		
		% finish()
		% Called by runSolver() when the simulation is done.
		% timeElapsed:	Time spent by the solver (returned from toc()).
		function finish(obj, timeElapsed)
			obj.timeElapsed = timeElapsed;
			
			obj.flushQueue();
			obj.solnMap = obj.solnMap(1:obj.numSoln, :);
		end
				
		
		% Iterator interface
		% Example of usage of iterator interface:
		% for itr = 1 : soln.end()
		%     [t, U] = soln.get(itr);
		%     (....)
		% end		
		function ret = end(obj)
			ret = obj.numSoln;
		end
		
		function [t, U] = get(obj, itr, removeGC)
			t = obj.solnMap{itr, 1};
			U = obj.solnMap{itr, 2};
			% Remove ghost cells if removeGC==true (default value is true)
			if nargin < 3 || removeGC
				U = obj.mesh.removeGhostCells(U);
			end
		end
		
		
		% Solution access utility functions
		% Use these instead of the solution iterator interface (get(), end()).
		
		% getFinal()
		% Returns the solution at the last timestep
		function [t, U] = getFinal(soln)
			[t, U] = soln.get(soln.end());
		end
		
		% getAtTime()
		% Returns the solution at time T.
		% The function finds the time step closest to 'T'. To get the last
		% solution, call 
		% 	getAtTime(soln, inf);
		% If the solution list is empty then t=-inf, U=[] is returned.
		function [t, U] = getAtTime(obj, T)
			% List of recorded times
			tList = cell2mat(obj.solnMap(:,1));
			if isempty(tList)
				t = -inf;
				U = [];
			else
				% Find closest time
				[dt, ind] = min(abs(tList-T));
				[t, U] = obj.get(ind);			
				% Remove ghost cells from solution
				U = obj.mesh.removeGhostCells(U);
			end
		end
	end
	
	
	
	
	methods (Access = private)
		% flushQueue()
		% Picks out timesteps from the solution queue and adds them to the
		% solution map, based on how many timesteps are to be included in
		% the final solution.
		function flushQueue(obj)
			% Pick out timesteps that will be written
			t = cell2mat(obj.solnMapQueue(1:obj.numQueue, 1));
			t2 = t(end);
			indIncl = obj.tInclude <= t2;
			tIncludeLocal = obj.tInclude(indIncl);
			if length(tIncludeLocal) < length(t)
				indNear = Utility.findNearest(tIncludeLocal, t);
			else
				indNear = 1:length(t);
			end
						
			% Add entries to solution list
			numAdd = length(indNear);
			obj.solnMap(obj.numSoln+1:obj.numSoln+numAdd, :) = ...
				obj.solnMapQueue(indNear, :);
			obj.numSoln = obj.numSoln + numAdd;

			% Remove timesteps that have been written
			obj.tInclude = obj.tInclude(~indIncl);
						
			% Create an empty solution queue, keeping the last timestep if
			% it has not been written yet.
			newQueue = obj.newSolnMap();
			% Keep the last timestep?
			if t(end) > t(indNear(end))
				newQueue(1,:) = obj.solnMapQueue(obj.numQueue,:);
				obj.numQueue = 1;
			else
				obj.numQueue = 0;
			end
			obj.solnMapQueue = newQueue;
		end
						
		
		% memUsage()
		% Gives an estimate on the amount of memory used by the program (in
		% MB). Used to decide whether to flush the solution queue or not.
		function ret = memUsage(obj)
			% TODO: Implement more refined version for Windows systems
			% using memory().
			
			% Only measuring the memory occupied by the solution queue
			% makes for an inaccurate estimate of the total amount of
			% memory used, but avoids flushing of the queue when the
			% solution map itself is bigger than maxMemUsage.
			
			
			% Accurate but slow method
% 			U = obj.solnMapQueue(:,2);	%#ok, refered to in call to whos()
% 			w = whos('U');
% 			ret = w.bytes / (2^20);

			ret = obj.mesh.nelem * obj.mesh.nxTot * obj.numSoln;
		end
				
		
		% newSolnMap()
		% Create a new, empty solution map object.
		function ret = newSolnMap(obj)
			ret = cell(100000, 2);
		end
	end
end
