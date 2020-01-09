% Computes trace(X^{-1}).
function cvx_optval = trace_inv(X)

I = eye(size(X));
cvx_begin
	variable Y(size(X)) symmetric;
	minimize(trace(Y))
	subject to
		[Y I; I X] == semidefinite(2*size(X));
cvx_end
