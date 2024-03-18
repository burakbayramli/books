function fvalue = fkn_exact(M,A,gamma,Astar)

fvalue = 1/M^2*(2/(gamma+1)*(1+(gamma-1)/2*M^2))^...
	 ((gamma+1)/(gamma-1))-(A/Astar)^2;

