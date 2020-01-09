clear all;
cvx_quiet(true);
m = 50;
n = 10;

randn('seed', 2);
A1 = randn(m,n+1);
A2 = randn(m,n+1);
b1 = randn(m,1);
b2 = randn(m,1);

cvx_begin
	variables x1(n) x2(n) y
	minimize (max(A1*[x1; y] + b1) + max(A2*[x2; y] + b2))
cvx_end
saved_y = y;
saved_optval = cvx_optval;

% choose one of these.
ALL = 0;
PRIMALSWEEP = 1;
PRIMAL = 0;
DUALSWEEP = 0;
DUAL = 1;
if PRIMALSWEEP || ALL
	close all;
	i = 1;
	Y = linspace(-1,1,50);
	for y = Y
		cvx_begin
			variable x1(n)
			minimize (max(A1*[x1; y] + b1))
		cvx_end
		p1(i) = cvx_optval;

		cvx_begin
			variable x2(n)
			minimize (max(A2*[x2; y] + b2))
		cvx_end
		p2(i) = cvx_optval;

		i = i + 1;
	end
	figure(1);
	cla;
	plot(Y, p1, 'b-.', 'linewidth', 1.5);
	hold on;
	plot(Y, p2, 'r', 'linewidth', 1.5);
	plot(Y, p1+p2, 'k--', 'linewidth', 1.5);
	legend('phi1', 'phi2', 'phi1(y) + phi2(y)');
	xlabel('y');
	set(gca, 'FontSize', 16);
	set(legend, 'FontSize', 19);
	print -depsc2 simple_sweep.eps
end
if PRIMAL || ALL
	close all;
	yl = -1; yu = 1;
	
	iters = 8 % 10th iteration a bit dodgy
	for i = 1:iters
		y = 0.5*(yl + yu)

		cvx_begin
			variable x1(n)
			variable yt
			dual variable nu1
			minimize (max(A1*[x1; yt] + b1))
			nu1 : yt == y
		cvx_end
		p1 = cvx_optval;

		cvx_begin
			variable x2(n)
			variable yt
			dual variable nu2
			minimize (max(A2*[x2; yt] + b2))
			nu2 : yt == y
		cvx_end
		p2 = cvx_optval;

		if (nu1 + nu2) < 0
			yl = y;
		else
			yu = y;
		end

		val(i) = p1 + p2;
		ys(i) = y;
	end

	figure(1);
	cla;
	semilogy(val - saved_optval, 'linewidth', 1.5);
	xlabel('k');
	ylabel('f - fmin');
	set(gca, 'FontSize', 16);
	print -depsc2 simple_primal.eps
end
if DUALSWEEP || ALL
	close all;
	i = 1;
	Nu = linspace(-1,1,50);
	for nu = Nu
		cvx_begin
			variable x1(n)
			variable y1
			minimize (max(A1*[x1; y1] + b1) + nu*y1)
		cvx_end
		g1(i) = cvx_optval;
			
		cvx_begin
			variable x2(n)
			variable y2
			minimize (max(A2*[x2; y2] + b2) - nu*y2)
		cvx_end
		g2(i) = cvx_optval;

		i = i + 1;
	end
	figure(1);
	cla;
	plot(Nu, g1, 'b-.', 'linewidth', 1.5);
	hold on;plot(Nu, g2, 'r', 'linewidth', 1.5);
	plot(Nu, g1+g2, 'k--', 'linewidth', 1.5);
	legend('g1(n)', 'g2(n)', 'g1(n) + g2(n)');
	xlabel('n');
	axis([-1 1 0 2.5]);
	set(gca, 'FontSize', 16);
	set(legend, 'FontSize', 19);
	print -depsc2 simple_dual_sweep.eps
end
if DUAL || ALL
	close all;
	nul = -1; nuu = 1;
	
	iters = 15;
	for i = 1:iters
		nu = 0.5*(nul + nuu)

		cvx_begin
			variable x1(n)
			variable y1
			minimize (max(A1*[x1; y1] + b1) + nu*y1)
		cvx_end
		g1 = cvx_optval;

		cvx_begin
			variable x2(n)
			variable y2
			minimize (max(A2*[x2; y2] + b2) - nu*y2)
		cvx_end
		g2 = cvx_optval;

		if (y2 - y1) < 0
			nul = nu;
		else
			nuu = nu;
		end

		val(i) = g1 + g2;
		nus(i) = nu;

		y = 0.5*(y1 + y2);
		worse(i) = max(A1*[x1; y] + b1) + max(A2*[x2; y] + b2);

		cvx_begin
			variable x1(n)
			minimize (max(A1*[x1; y] + b1))
		cvx_end
		g1 = cvx_optval;

		cvx_begin
			variable x2(n)
			minimize (max(A2*[x2; y] + b2))
		cvx_end
		g2 = cvx_optval;
		better(i) = g1 + g2;
	end

	figure(1);
	cla;
	plot(better, 'b-.', 'linewidth', 1.5)
	hold on;
	plot(worse, 'r--', 'linewidth', 1.5);
	plot(val, 'k', 'linewidth', 1.5);
	xlabel('k');
	legend('better bound', 'worse bound', 'dual function value');
	set(gca, 'FontSize', 16);
	set(legend, 'FontSize', 19);
	print -depsc2 simple_dual.eps
end
