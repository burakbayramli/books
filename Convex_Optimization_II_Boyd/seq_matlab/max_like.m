% ncqp.m
% Heuristic for a maximum likelihood problem.
% Convex-concave procedure example.
% EE364b, Convex Optimization II, S. Boyd, Stanford University.
% Written by Jacob Mattingley, 2008-04.

% Note: instead of trace(E^-1*Y), use the symmetric version
% trace(Y^(1/2)*E^-1*Y^(1/2)). To calculate that, use the equivalent problem
% minimize trace(Z), subject to [Z Y^(1/2); Y^(1/2) E] >= 0.

randn('state', 0); rand('state', 2847); n = 10; N = 15; Kmax = 7;
Etrue = rand(n); Etrue = Etrue'*Etrue; Etrue = Etrue/max(Etrue(:));

% Generate some samples.
X = sqrtm(Etrue)*randn(n, N);

figure(1); cla;
for i = 1:5
	objs = [];
	Y = (1/N)*X*X';
	Yhalf = sqrtm(Y);
	Ek = rand(n);
	Ek = Ek'*Ek;
	for k = 1:Kmax
		cvx_begin
			cvx_quiet(true);
			variable E(n, n) symmetric;
			variable Z(n, n) symmetric;
			minimize(log_det(Ek) + trace(inv(Ek)*(E - Ek)) + trace(Z))
			[Z Yhalf; Yhalf E] == semidefinite(2*n);
			E >= 0;
			E == semidefinite(n)
		cvx_end
		disp(cvx_status);
		Ek = E;
		objs = [objs log_det(Ek) + trace(inv(Ek)*(E - Ek)) + trace(inv(E)*Y)];
	end

	figure(1); plot(objs); hold on; drawnow;
end

figure(1);
xlabel('x'); ylabel('y');
axis auto; a = axis; a(1) = 1; a(2) = Kmax; axis(a);
set(gca, 'FontSize',18);
print -deps2 mlobjs.eps
