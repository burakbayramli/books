function [xbar,p,e] = lsq(A,b)
%LSQ	Least squares.
%	[xbar,p,e] = LSQ(A,b) finds a least squares
%	solution to the overdetermined system A*x ~= b.
%	xbar = the solution to the normal equations.
%	p = projection of b onto the column space.
%	e = b - p.
%	
if determ(A'*A) == 0
   error('Columns of A are linearly dependent')
end
xbar = solve(A'*A,A'*b);
p = A*xbar;
e = b - p;
