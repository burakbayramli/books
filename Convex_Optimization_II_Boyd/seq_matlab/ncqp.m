% ncqp.m
% Heuristic for minimizing a nonconvex quadratic program.
% Sequential convex optimization example.
% EE364b, Convex Optimization II, S. Boyd, Stanford University.
% Written by Jacob Mattingley, 2008-04.

randn('state', 1029); rand('state', 1029);
n = 15; Kmax = 30; Nmax = 10;
P = randn(n);
P = P + P'; % symmetric but not PSD.
q = randn(n, 1);

% Compute lower bound via Lagrange dual.
cvx_begin
	variable lam(n);
	maximize -0.5*(matrix_frac(q, P + diag(lam)) + sum(lam))
	lam >= 0
cvx_end
lowerbound = cvx_optval;

% minimize (1/2)*x'*P*x + q'*x, subject to norm(x, inf) <= 1.

figure(1); cla; cvx_quiet(true);
for i = 1:Nmax
	% Choose a random starting point.
    xk = 2*(rand(n, 1) - 0.5);

    fs = zeros(Kmax, 1);
    for k = 1:Kmax
        fxk = (1/2)*xk'*P*xk + q'*xk;
        cvx_begin
            [V, D] = eig(P);
            Pp = V*pos(D)*V';

            variable x(n);

            minimize(fxk + (P*xk + q)'*(x - xk) + (1/2)*quad_form(x - xk, Pp))
            norm(x, inf) <= 1;
			abs(x - xk) <= 0.2
        cvx_end
        disp(cvx_status); disp(cvx_optval);
        fs(k) = cvx_optval;

		% Stop if we have changed less than 0.1%.
        if norm(xk - x) <= 0.001*norm(x)
            fs(k+1:end) = cvx_optval;
            break;
        end
        xk = x;
    end
    plot(fs); hold on; drawnow;
end

if 0 % produce graphs.
	axis([1 Kmax -70 -10])
	plot([1 Kmax], [lowerbound lowerbound], 'k--')
	xlabel('x')
	ylabel('y')
	print -deps ncqp.eps
end
