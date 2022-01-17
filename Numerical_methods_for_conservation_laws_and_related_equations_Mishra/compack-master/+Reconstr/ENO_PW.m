classdef ENO_PW < Reconstr.ENO
	%ENO_PW   Point-wise (as opposed to cell-averaged) ENO
	
	
	methods
		function obj = ENO_PW(k)
			obj = obj@Reconstr.ENO(k, '+Reconstr/ENO_coeff_PW');
		end
	end
	
end

