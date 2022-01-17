classdef WENO < Reconstr.ENO
	%WENO   Weighted essentially nonoscillatory reconstruction

	
	properties (Access=private)
		drL
		drR
		epsilon
	end
	
	
	
	methods
		%% WENO()
		function obj = WENO(k)
			% Run ENO base class constructor
			obj = obj@Reconstr.ENO(k);
			% Set stencil weights
			drAll = { 1, [2/3; 1/3], [3/10; 3/5; 1/10] };
			obj.drR = drAll{k};
			obj.drL = obj.drR(k:-1:1);
			% Set division-by-zero guard
			obj.epsilon = 1e-6;
		end
	end
	
	
	
	methods(Access = protected)
		%% doReconstr()
		% Performs the actual ENO reconstruction on a vector "u"
		function [ul, ur] = doReconstr(obj, u, mesh)
			%% Initialize
			% Total number of cells			
			n = length(u);
			% Number of internal grid cells
			nInt = n - 2*(obj.k-1);
			
			
			%% Compute stencil weights
			b = obj.smoothnessIndicator(u);
			weightL = zeros(obj.k, n);
			weightR = zeros(obj.k, n);
			stencils = obj.k*ones(obj.k, n);
			for m = 1 : nInt
				l = m+obj.k-1;
				
				% Compute stencil weights
				alphaL = obj.drL ./ (obj.epsilon+b(:, m)).^2;
				alphaR = obj.drR ./ (obj.epsilon+b(:, m)).^2;
				weightL(:, l) = alphaL ./ sum(alphaL);
				weightR(:, l) = alphaR ./ sum(alphaR);
				
				% Set the current stencil index				
				stencils(:, l) = l : l+obj.k-1;
			end
			
			
			%% Compute reconstructed values
			ulAll = zeros(obj.k, n);
			urAll = zeros(obj.k, n);
			for r = 0:obj.k-1
				ulAll(r+1,:) = obj.Crj(:, r+1)' * u(stencils-r);
				urAll(r+1,:) = obj.Crj(:, r+2)' * u(stencils-r);
% 				curR = r*ones(1, n);
% 				[ulAll(r+1,:), urAll(r+1,:)] = ...
% 					obj.interpolate(u, curR, stencils-r);
			end
			ul = dot(ulAll, weightL);
			ur = dot(urAll, weightR);
		end
		
		
		
		%% smoothessIndicator()
		function b = smoothnessIndicator(obj, u)
			n = length(u);
			r = obj.k : n-obj.k+1;
			if obj.k == 2
				b = [(u(r+1)-u(r)).^2;
					(u(r)-u(r-1)).^2];
			elseif obj.k == 3
				b = [13/12*(u(r) - 2*u(r+1) + u(r+2)).^2 + 1/4*(3*u(r) - 4*u(r+1) + u(r+2)).^2;
					13/12*(u(r-1) - 2*u(r) + u(r+1)).^2 + 1/4*(u(r-1) - u(r+1)).^2;
					13/12*(u(r-2) - 2*u(r-1) + u(r)).^2 + 1/4*(3*u(r) - 4*u(r-1) + u(r-2)).^2];
			else
				error('WENO only implemented for k=2 and 3.');
			end
		end
	end
	
end

