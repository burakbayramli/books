clear all;
cvx_quiet(true);

m = 30;
n = 10;

randn('seed', 1);
rand('seed', 1);

A1 = randn(m,n);
A2 = randn(m,n);
b1 = rand(m,1); % zero is feasible.
b2 = rand(m,1);
c1 = randn(n,1);
c2 = randn(n,1);
D1 = randn(2,n);
D2 = randn(2,n);

cvx_begin
		variables x1(n) x2(n)
		dual variable lambda
		minimize (c1'*x1 + c2'*x2 + 0.1*x1'*x1 + 0.1*x2'*x2)
		A1*x1 <= b1
		A2*x2 <= b2
		lambda : D1*x1 + D2*x2 <= 0;
cvx_end
saved_optval = cvx_optval;

% pick one of these.
ALL = 1;
PRIMAL = 0;
DUAL = 1;

if PRIMAL || ALL
	close all;
	niters = 100;
	ts = zeros(2,niters);
	t = zeros(2,1);
	for i = 1:niters
		cvx_begin
			variable x1(n)
			dual variable l1
			minimize (c1'*x1 + 0.1*x1'*x1)
			A1*x1 <= b1
			l1 : D1*x1 <= t
		cvx_end
		f1 = cvx_optval;

		cvx_begin
			variable x2(n)
			dual variable l2
			minimize (c2'*x2 + 0.1*x2'*x2)
			A2*x2 <= b2
			l2 : D2*x2 <= -t
		cvx_end
		f2 = cvx_optval;


		alpha = 0.1;
		t = t + alpha*(l1 - l2);
		
		f1 + f2
		f(i) = f1 + f2;
		ts(:,i) = t;
	end

	figure(1);
	cla;
	semilogy(f - saved_optval, 'linewidth', 1.5);
	xlabel('k');
	ylabel('f - fmin');
	set(gca, 'FontSize', 16);
	print -depsc2 resource_primal.eps

	figure(2);
	cla;
	plot(ts(1,:), 'b', 'linewidth', 1.5);
	hold on;
	plot(ts(2,:), 'r', 'linewidth', 1.5);
	xlabel('k');
	set(gca, 'FontSize', 16);
	print -depsc2 resource_primal2.eps
end
if DUAL || ALL
	close all;
	niters = 30;

	lambda = zeros(2,1);
	lambdas = zeros(2,niters);
	subgs = zeros(2,niters);
	gs = zeros(niters,1);
	feas = zeros(niters,1);
	for i = 1:niters
		cvx_begin
			variable x1(n)
			minimize (c1'*x1 + 0.1*x1'*x1 + lambda'*D1*x1)
			A1*x1 <= b1
		cvx_end
		g1 = cvx_optval;

		cvx_begin
				variable x2(n)
				minimize (c2'*x2 + 0.1*x2'*x2 + lambda'*D2*x2)
				A2*x2 <= b2
		cvx_end
		g2 = cvx_optval;

		subg = D1*x1 + D2*x2; % subgradient of negative dual function
		subgs(:,i) = subg;

		alpha = 0.5/i;
		lambda = max(lambda + alpha*(subg), 0);
		lambdas(:,i) = lambda;
		% next line not very interesting.
		feas(i) = (subg(1) <= 0) && (subg(2) <= 0);
		g(i) = g1 + g2;

		% Now solve primal problems.
		t1 = D1*x1 - 0.5*max(subg , 0);
		t2 = D2*x2 - 0.5*max(subg , 0);
		cvx_begin
			variable x1(n)
			minimize (c1'*x1 + 0.1*x1'*x1)
			A1*x1 <= b1
			D1*x1 <= t1;
		cvx_end
		g1 = cvx_optval;

		cvx_begin
			variable x2(n)
			minimize (c2'*x2 + 0.1*x2'*x2)
			A2*x2 <= b2
			D2*x2 <= t2;
		cvx_end
		g2 = cvx_optval;

		prim(i) = g1 + g2;
	end

	figure(1);
	cla reset;
	plot(g, 'k', 'linewidth', 1.5);
	hold on;
	plot(prim, 'b--', 'linewidth', 1.5);
	xlabel('k');
	legend('g(L)', 'fh');
	set(gca, 'FontSize', 16);
	set(legend, 'FontSize', 20);
	print -depsc2 resource_dual.eps

	figure(2);
	cla reset;
	semilogy(saved_optval - g, 'k', 'linewidth', 1.5);
	hold on;
	semilogy(prim - g, 'b--', 'linewidth', 1.5);
	axis([0 30 1e-5 1e1]);
	xlabel('k');
	legend('fst - g(L)', 'f - g(L)');
	set(gca, 'FontSize', 16);
	set(legend, 'FontSize', 20);

	print -depsc2 resource_dual2.eps

	figure(3);
	cla reset;
	plot(lambdas(1,:), 'b', 'linewidth', 1.5);
	hold on;
	plot(lambdas(2,:), 'r', 'linewidth', 1.5);
	xlabel('k');
	set(gca, 'FontSize', 16);
	print -depsc2 resource_dual3.eps
end
