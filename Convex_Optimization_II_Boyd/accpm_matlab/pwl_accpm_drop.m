% Piecewise linear minimization via ACCPM -
% dropping constraints and keeping N = 3n 
% EE364b example
% 
% PWL minimization problem: min max_i=1..m (a_i'x + b_i)
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
% cutting plane method (no constraint dropping) 
%********************************************************************
% number of iterations 
niter = 200; 
% initial point 
x = zeros(n,1); 
% initial localization polyhedron {x | Cx <= d}
R = 1; 
C = [eye(n); -eye(n)];             
d = R*ones(2*n,1); 

f_save1 = []; f_best1 = []; l_save1 = []; l_best1 = []; 
x_save = x; 
ncstr1 = 2*n;                           % number of constraints 
flop1 = [];                             % approx. flop count per iter
for iter = 1:niter 
    disp(iter)
    % find analytical center of polyhedron
    [x, H, nt] = acent(C,d,x);   
    % compute approx flop count for centering step
    flop1 = [flop1 nt*n.^2*length(d)];
    % find active functions at current x
    [f, idx] = max(A*x + b);        
    % subgradient at current x
    g = A(idx(1),:)';  
    % save current and best function values 
    f_save1 = [f_save1 f];
    f_best1 = [f_best1 min(f_save1)];
    % update polyhedron 
    C = [C; g']; 
%     d = [d; g'*x];                         % neutral cut
    d = [d; g'*x + f_best1(end) - f];        % deep cut    
    ncstr1 = [ncstr1 length(d)];
end

%********************************************************************
% cutting plane method with dropping constraints
%********************************************************************
% number of iterations 
niter = 200; 
% initial point 
x = zeros(n,1); 
% initial localization polyhedron {x | Cx <= d}
R = 1; 
C = [eye(n); -eye(n)];             
d = R*ones(2*n,1); 
N = 3*n; 

f_save2 = []; f_best2 = []; l_save2 = []; l_best2 = []; 
x_save = x; 
ncstr2 = 2*n;                           % number of constraints 
flop2 = [];                             % approx. flop count per iter
for iter = 1:niter 
    disp(iter)
    % find analytical center of polyhedron
    [x, H, nt] = acent(C,d,x);        
    % compute approx flop count for centering step
    flop2 = [flop2 nt*n.^2*length(d)];    
    % ranking and dropping constraints 
    temp1 = C*x - d; 
    temp2 = m*sqrt(diag(C*(H\C'))); 
    r = temp1./temp2; 
    [r_sort, ind] = sort(r, 1, 'descend'); 
    if length(d) > N -1 
        C = C(ind(1:N-1),:); 
        d = d(ind(1:N-1)); 
    end
    % find active functions at current x
    [f, idx] = max(A*x + b);        
    % subgradient at current x
    g = A(idx(1),:)';  
    % save current and best function values 
    f_save2 = [f_save2 f];
    f_best2 = [f_best2 min(f_save2)]; 
    % update polyhedron 
    C = [C; g']; 
%     d = [d; g'*x];                         % neutral cut
    d = [d; g'*x + f_best2(end) - f];        % deep cut    
    ncstr2 = [ncstr2 length(d)];    
end

%********************************************************************
% plots 
%********************************************************************
figure
set(gca, 'FontSize',18);
semilogy(f_save1 - f_star)
hold on 
semilogy(f_save2 - f_star, 'r-.')
xlabel('k'); 
ylabel('f-fmin');
legend('no dropping','keep 3n')
print -depsc pwl_accpm_drop

figure
set(gca, 'FontSize',18);
semilogy(f_best1 - f_star)
hold on
semilogy(f_best2 - f_star, 'r-.')
xlabel('k');
ylabel('fbest-fmin');
legend('no dropping','keep 3n')
print -depsc pwl_accpm_drop_best

figure
set(gca, 'FontSize',18);
plot(ncstr1);
hold on 
plot(ncstr2, 'r-.');
xlabel('k')
ylabel('nbr constraints') 
legend('no dropping','keep 3n','Location','NorthWest')
print -depsc pwl_accpm_cstr

figure
set(gca, 'FontSize',18);
semilogy(cumsum(flop1)/1e6,f_best1-f_star);
hold on 
semilogy(cumsum(flop2)/1e6, f_best2-f_star, 'r-.');
xlabel('flop count')
ylabel('fbest-fmin')
legend('no dropping','keep 3n')
print -depsc pwl_accpm_drop_flop
