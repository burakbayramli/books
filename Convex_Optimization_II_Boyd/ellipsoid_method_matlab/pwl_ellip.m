% Piecewise linear minimization via ACCPM 
% EE364b example
% 
% PWL minimization problem: min max_i=1..m (a_i'x + b_i)
%

% number of variables n and linear functions m
n = 20; m = 100;

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
% ellipsoid method
%********************************************************************
eps = 0.01;
% number of iterations 
niter = 2000;
% initial ellipsoid
P = eye(n);
% initial point 
x = zeros(n,1); 
U = [+inf]; L = [-inf];
Lm = [-inf];

f_save = []; f_best = []; 
for iter = 1:niter 
    % find active functions at current x
    [f, idx] = max(A*x + b);        
    % subgradient at current x
    g = A(idx(1),:)';
	% convergence test
	if sqrt(g'*P*g) <= eps
		disp('converged')
		break
	end
    % save current and best function values 
    f_save = [f_save f];
    f_best = [f_best min(f_save)];

	% update upper / lower bounds.
	U(iter+1) = min(U(iter), max(A*x + b));
	L(iter+1) = max(L(iter), max(A*x + b) - sqrt(g'*P*g));
	Lm(iter+1) = max(A*x + b) - sqrt(g'*P*g);

	h = f - f_best(end);

	if DEEPCUT && h > 0
		% update with deep cut.
		alpha = h/sqrt(g'*P*g);
		gt = g/sqrt(g'*P*g);
		x = x - (1+alpha*n)/(n+1)*P*gt;
		P = n^2*(1-alpha^2)/(n^2-1)*(P-2*(1+alpha*n)/((n+1)*(1+alpha))*P*gt*gt'*P);
	else
		% update ellipsoid with shallow cut.
		gt = g/sqrt(g'*P*g);
		x = x - 1/(n+1)*P*gt;
		P = n^2/(n^2-1)*(P-2/(n+1)*P*gt*gt'*P);
	end
end

% trim U, L. Unfortunate hack.
U = U(2:end);
L = L(2:end);
Lm = Lm(2:end);
