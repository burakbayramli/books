% robot.m
% Sequential convex optimization example.
% Heuristic for minimizing sum of torques for a robot arm trajectory.
% EE364b, Convex Optimization II, S. Boyd, Stanford University.
% Written by Jacob Mattingley, 2008-04.

% Set simulation parameters.
m1 = 1; m2 = 5; l1 = 1; l2 = 1;
N = 40; T = 10; h = T/N;
startpos = [0 -2.9]'; endpos = [3 2.9]';
taumax = 1.1;
alpha = 0.1; betasucc = 1.1; betafail = 0.5; rhoinit = 90*pi/180;
lambda = 2;
Kmax = 40;

% Set the inital trajectory to linearly interpolate the two points.
ot = [startpos [linspace(startpos(1), endpos(1), N); ...
	  linspace(startpos(2), endpos(2), N)] endpos];
otdot = zeros(2, N+2);
otddot = zeros(2, N+2);
for t = 2:N+1
	otdot(:,t) = (ot(:,t) - ot(:,t-1))/h;
	otddot(:,t) = (ot(:,t+1) - 2*ot(:,t) + ot(:,t-1))/(h^2);
end

etas = zeros(Kmax, 1);
etahats = zeros(Kmax, 1);
Js = zeros(Kmax, 1);
phis = zeros(Kmax, 1);
rhos = zeros(Kmax, 1); rhos(1) = rhoinit;
phihats = zeros(Kmax, 1);
pdec = zeros(Kmax, 1);
adec = zeros(Kmax, 1);
cvx_quiet(true);

% Calculate initial value of phi.
eta = zeros(2, N);
for t = 2:N+1
	% Dynamics equations.
	t1 = ot(1,t); t2 = ot(2,t); t1dot = otdot(1,t); t2dot = otdot(2,t);
	M = [(m1 + m2)*l1^2 m2*l1*l2*(sin(t1)*sin(t2) + cos(t1)*cos(t2));
		 m2*l1*l2*(sin(t1)*sin(t2)+cos(t1)*cos(t2)) m2*l2^2];
	W = [0 m2*l1*l2*(sin(t1)*cos(t2) - cos(t1)*sin(t2))*t2dot;
		 m2*l1*l2*(sin(t1)*cos(t2) - cos(t1)*sin(t2))*t1dot 0;];
	eta(:,t-1) = 0 - M*otddot(:,t) - W*otdot(:,t);
end
oldphi = 0 + lambda*sum(abs(eta(:)));

for i = 1:Kmax
	cvx_begin
		variable nt(2, N+2);
		variable ntdot(2, N+2);
		variable ntddot(2, N+2);
		variable tau(2, N+2);
		etahat = cvx(zeros(2, N));

		% Initial and final conditions.
		nt(:,1) == startpos; nt(:,2) == startpos;
		nt(:,N+1) == endpos; nt(:,N+2) == endpos;
		tau(:, 1) == 0; tau(:, N+2) == 0;

		for t = 2:N+1
			% Consistency of first derivatives.
			ntdot(:,t) == (nt(:,t+1) - nt(:,t-1))/(2*h);

			% Consistency of double derivatives.
			ntddot(:,t) == (nt(:,t+1) - 2*nt(:,t) + nt(:,t-1))/(h^2);

			% Dynamics equations.
			t1 = ot(1,t); t2 = ot(2,t); t1dot = otdot(1,t); t2dot = otdot(2,t);
			M = [(m1 + m2)*l1^2 m2*l1*l2*(sin(t1)*sin(t2) + cos(t1)*cos(t2));
				 m2*l1*l2*(sin(t1)*sin(t2)+cos(t1)*cos(t2)) m2*l2^2];
			W = [0 m2*l1*l2*(sin(t1)*cos(t2) - cos(t1)*sin(t2))*t2dot;
				 m2*l1*l2*(sin(t1)*cos(t2) - cos(t1)*sin(t2))*t1dot 0;];
			etahat(:,t-1) = tau(:,t) - M*ntddot(:,t) - W*ntdot(:,t);
		end

		% Trust region constraints.
		abs(nt(:) - ot(:)) <= rhos(i);

		% Torque limit.
		abs(tau(:)) <= taumax;

		% Second arm can't fold back on first.
		nt(2, :) <= pi;
		nt(2, :) >= -pi;

		minimize(h*sum_square(tau(:)) + lambda*sum(abs(etahat(:))));
	cvx_end

	% Calculate *actual* torque violations.
	eta = zeros(2, N);
	for t = 2:N+1
		% Dynamics equations.
		t1 = nt(1,t); t2 = nt(2,t); t1dot = ntdot(1,t); t2dot = ntdot(2,t);
		M = [(m1 + m2)*l1^2 m2*l1*l2*(sin(t1)*sin(t2) + cos(t1)*cos(t2));
			 m2*l1*l2*(sin(t1)*sin(t2)+cos(t1)*cos(t2)) m2*l2^2];
		W = [0 m2*l1*l2*(sin(t1)*cos(t2) - cos(t1)*sin(t2))*t2dot;
			 m2*l1*l2*(sin(t1)*cos(t2) - cos(t1)*sin(t2))*t1dot 0;];
		eta(:,t-1) = tau(:,t) - M*ntddot(:,t) - W*ntdot(:,t);
	end

	etahats(i) = sum(abs(etahat(:))); etas(i) = sum(abs(eta(:)));
	Js(i) = h*sum_square(tau(:));

	phihats(i) = Js(i) + lambda*etahats(i); phis(i) = Js(i) + lambda*etas(i);

	deltahat = oldphi - phihats(i); delta = oldphi - phis(i);
	oldphi = phis(i);

	pdec(i) = deltahat; adec(i) = delta;

	if delta <= alpha*deltahat
		if i ~= Kmax
			rhos(i+1) = betafail*rhos(i);
		end

		% Undo recording of the last step.
		if i ~= 1
			Js(i) = Js(i-1);
			phis(i) = phis(i-1);
			etas(i) = etas(i-1);
			etahats(i) = etahats(i-1);
			oldphi = phis(i);
		end
	else
		if i ~= Kmax
			rhos(i+1) = betasucc*rhos(i);
		end
		% Propagate the system.
		ot = nt; otdot = ntdot;
	end

	disp(cvx_status);
	disp('       iter  hatphys  realphys    phihat       phi  deltahat     delta       rho')
	disp([i etahats(i) etas(i) phihats(i) phis(i) deltahat delta rhos(i)])


	if 0 % animation of arm.
		figure(1);
		p1 = l1*[cos(nt(1,:)); sin(nt(1,:))];
		p2 = p1 + l2*[cos(nt(1,:) + nt(2,:)); sin(nt(1,:) + nt(2,:))];

		plot([0 p1(1,1) p2(1,1)], [0 p1(2,1) p2(2,1)], 'k-', 'linewidth', 3);
		axis([-2.2 2.2 -2.2 2.2]);
		axis square;
		pause(0.2);

		for t = 1:N
			plot([0 p1(1,t) p2(1,t)], [0 p1(2,t) p2(2,t)], 'k-', 'linewidth', 3);
			axis([-2.2 2.2 -2.2 2.2]);
			axis square;
			pause(0.05)
		end
	end
end

if 0 % produce graphs.
	% phi.
	figure(1); cla;
	plot(phis, 'k-');
	axis auto; a = axis; a(1) = 1; a(2) = Kmax; axis(a);
	xlabel('x'); ylabel('y');
	set(gca, 'FontSize', 18);
	print -deps phis.eps

	% J.
	figure(2);
    plot(Js, 'k-')
	axis auto; a = axis; a(1) = 1; a(2) = Kmax; axis(a);
    ylabel('J'); xlabel('k');
	set(gca, 'FontSize', 18);
	print -deps J.eps

	% Average torque residual.
	figure(3);
    semilogy(etas, 'k-')
	axis auto; a = axis; a(1) = 1; a(2) = Kmax; axis(a);
	xlabel('x'); ylabel('y');
	set(gca, 'FontSize', 18);
	print -deps avgphysres.eps

	% rho (in degrees).
	figure(4);
	semilogy(180/pi*rhos, 'k-');
	axis auto; a = axis; a(1) = 1; a(2) = Kmax; axis(a);
	xlabel('x'); ylabel('y');
	set(gca, 'FontSize', 18);
	print -deps rho.eps

	% Torques.
	figure(5);
	subplot(211);
	plot(linspace(0,T,N), tau(1,2:end-1));
	xlabel('x1'); ylabel('y1');
	subplot(212);
	plot(linspace(0,T,N), tau(2,2:end-1));
	xlabel('x2'); ylabel('y2');
	set(gca, 'FontSize', 18);
	print -deps torques.eps
	
	% Thetas.
	figure(6);
	subplot(211);
	plot(linspace(0,T,N), nt(1,2:end-1));
	xlabel('x1'); ylabel('y1');
	subplot(212);
	plot(linspace(0,T,N), nt(2,2:end-1));
	xlabel('x2'); ylabel('y2');
	set(gca, 'FontSize', 18);
	print -deps thetas.eps

	% Predicted vs actual decrease.
	figure(7);
	cla;
	plot(pdec, 'k--');
	hold on;
	plot(adec, 'k-');
	axis auto; a = axis; a(1) = 1; a(2) = Kmax; axis(a);
	xlabel('x'); ylabel('y');
	set(gca, 'FontSize', 18);
	print -deps decreases.eps
end
