% Piecewise linear minimization via ACCPM 
% EE364b example
% 
% PWL minimization problem: min max_i=1..m (a_i'x + b_i)
%

% number of variables n and linear functions m
n = 20; m = 100;

%DEEPCUT = 1;

%********************************************************************
% generate an example
%********************************************************************
randn('state',1)
A = randn(m,n);
b = randn(m,1);

%********************************************************************
% compute pwl optimal point using CVX
%********************************************************************
cvx_begin
    variable x(n)  
    minimize max(A*x + b)
cvx_end
f_star = cvx_optval;

%********************************************************************
% epigraph ellipsoid method
%********************************************************************
% number of iterations 
niter = 2000;
% initial point 
x = zeros(n,1); t = 3;
% make n++ because we added t as a new variable.
n = n + 1;
% initial ellipsoid (quite large)
P = eye(n); P(end,end) = 10;
U = [+inf]; L = [-inf];
Lm = [-inf];

f_save = []; f_best = []; 
for iter = 1:niter
	% find active functions at current x
	[f, idx] = max(A*x + b);        

	if f <= t
		% we have a feasible point. make objective cut
		g = [zeros(n-1,1); 1];
		h = t - f;
		alpha = h/sqrt(g'*P*g);
		gt = g/sqrt(g'*P*g);
		tmp = [x; t] - (1+alpha*n)/(n+1)*P*gt;
		x = tmp(1:end-1); t = tmp(end);
		P = n^2*(1-alpha^2)/(n^2-1)*(P-2*(1+alpha*n)/((n+1)*(1+alpha))*P*gt*gt'*P);
	elseif f > t
		% subgradient at current x. make feasibility cut.
		g = [A(idx(1),:)'; -1];
		h = f - t;
		alpha = h/sqrt(g'*P*g);
		gt = g/sqrt(g'*P*g);
		tmp = [x; t] - (1+alpha*n)/(n+1)*P*gt;
		x = tmp(1:end-1); t = tmp(end);
		P = n^2*(1-alpha^2)/(n^2-1)*(P-2*(1+alpha*n)/((n+1)*(1+alpha))*P*gt*gt'*P);
	end

    % save current and best function values 
    f_save = [f_save f];
    f_best = [f_best min(f_save)];

	% update upper / lower bounds.
	U(iter+1) = min(U(iter), f);
	L(iter+1) = max(L(iter), f - sqrt(g'*P*g));
	Lm(iter+1) = t - sqrt(g'*P*g);

end

% trim U, L.
U = U(2:end);
L = L(2:end);
Lm = Lm(2:end);
