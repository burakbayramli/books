% Piecewise linear minimization via ACCPM (epigraph form) 
% EE364b example
% 
% PWL minimization problem: min max_i=1..m (a_i'x + b_i)
% which is equivalent to 
%                           min t 
%                          s.t. Ax + b <= t
%
% Written by Joelle Skaf 

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
% Epigraph cutting plane method 
%********************************************************************
% number of iterations 
niter = 50; 
% starting point 
x = zeros(n,1); 
R = 1; 
[t, idx] = max(b);                      % f(0) 
g0 = A(idx(1),:)';                      % gradient at x = 0 
lb = t - R*norm(g0,1);                  % lower bound on t 
% Initial polyhedron {x | Cx <= d} in R^{n+1} 
C = [eye(n+1); -eye(n+1)];              
d = [ones(n,1); t+1; ones(n,1); -lb]; 
            
f_save = []; f_best = [];
z = [x; t];
tot_nt = [];
for iter = 1:niter 
    disp(iter)
    % find analytical center of polyhedron
    [z, H, nt] = acent(C,d,z); 
    x = z(1:n);
    t = z(end);
    tot_nt = [tot_nt nt];      
    % find value of f at current x 
    [f, idx] = max(A*x + b);            % active functions at current x
    g = A(idx(1),:)';                   % subgradient at current x
    f_save = [f_save f];
    f_best = [f_best min(f_save)];
    % update z to be on the epigraph
    z = [x; f];
    % update polyhedron
    g1 = [g; -1];
    g2 = [zeros(n,1);1]; 
    C = [C; g1'; g2'];
    d = [d; g'*x - f; f];    
end

%********************************************************************
% plots 
%********************************************************************
figure
set(gca, 'FontSize',18);
semilogy(f_save - f_star)
xlabel('k'); 
ylabel('f-fmin');
print -depsc pwl_accpm_epi
figure
set(gca, 'FontSize',18);
semilogy(f_best - f_star)
xlabel('k'); 
ylabel('fbest-fmin');
print -depsc pwl_accpm_epi_best
figure 
set(gca, 'FontSize',18);
semilogy(cumsum(tot_nt), f_best - f_star)
xlabel('k'); 
ylabel('fall-fmin');
print -depsc pwl_accpm_epi_all 

    
