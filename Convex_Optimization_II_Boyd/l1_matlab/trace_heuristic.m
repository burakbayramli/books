% EE364B example for l1 extensions lectures 
% written by Joelle Skaf 03/07
%
% Given a matrix \Sigma, find an approximation of the form 
% \hat\Sigma = D + FF^T where D is diagonal nonnegative and F is nxr. 
% Let X = FF^T and 
%           minimize      rank(X)
%           subject to    X >= 0, D >= 0 diagonal 
% Applying the trace heuristic 
%           minimize      tr(X) 
%           subject to    X >= 0, D >= 0 diagonal 

randn('state',0);
rand('state',0);
n = 20; 
r = 3; 

% generate true data
F_true = randn(n,r); 
d_true  = 10*rand(n,1); 
Sigma = diag(d_true) + F_true*F_true'; 

N = 3000; 
Semp = 0; 
for i=1:N
    x = sqrtm(Sigma)*randn(n,1); 
    Semp = Semp + x*x'; 
end
Semp = Semp/(N-1); 
Shalf = sqrtm(Semp); 

% reconstruct data using trace heuristic 
betas = logspace(-2,log10(0.5),10); 
ranks = []; eigs  = []; Xs = []; ds = [];

for j = 1:length(betas) 
    cvx_begin
        variable d(n) 
        variable X(n,n) symmetric 
        minimize (trace(X))
        d>=0 
        X==semidefinite(n);
        norm(inv(Shalf)*(X+diag(d) - Semp)*inv(Shalf)) <= betas(j)
    cvx_end 
    disp([j sum(eig(X)>1e-3)])
    eigs  = [eigs eig(X)]; 
    ranks = [ranks sum(eig(X)>1e-3)]; 
    Xs{j} = X; 
    ds    = [ds d];  
end

% plots 
figure(1);
set(gca, 'FontSize',18);
semilogx(betas,ranks); hold on; semilogx(betas, ranks, '.')
xlabel('beta'); ylabel('rank(X)');
print -depsc trace_heuristic_rank 

figure(2);
set(gca, 'FontSize',18);
loglog(betas, eigs); hold on; loglog(betas, eigs,'.'); 
xlabel('beta'); ylabel('eig(X)'); axis([0.01 1 1e-5 100]);
print -depsc trace_heuristic_eig

% find "knee" of tradeoff curve 
idx = find(ranks == r); 
beta_c = betas(idx(1)); 
X_c = Xs{idx(1)}; 
d_c = ds(:,idx(1)); 

% angle between subspace spanned by X and subspace spanned by FF'
U = orth(F_true); 
[V, e] = eig(X_c); 
V = V(:,end-r+1:end); 
c = min(svd(U'*V));
disp('Angle between subspace spanned by X and subspace spanned by FF^T is:');
disp(acos(c)*180/pi);

% RMS value of difference between d and d_true 
disp('RMS value of difference between d and d_true: ') 
disp(norm(d_c-d_true)/norm(d_true)); 
