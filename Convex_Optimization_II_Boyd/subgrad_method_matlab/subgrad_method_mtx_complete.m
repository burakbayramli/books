% Positive semidefinite matrix completion
%   find        X
%   subject to  X(i,j) = val,   where (i,j,val) is the data 
%               X is PSD mtx with size n-by-n
% via alternating projections method.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07
%

% generate a problem instance
n = 50; % matrix size
p = 500; % number of missing entries
% generate a positive definite matrix
randn('state',1); % set state so problem is reproducable
A = randn(n,n);
X = A'*A;
fprintf(1,'Constructed PD mtx with min eig = %3.4f\n',min( eig(X) ));

% getting a random sparsity pattern
rand('state',1);
S = rand(n,n); 
% make sure sparsity pattern is symmetric and has ones on the diagonal
S = (S + S') + eye(n,n) > 1;

% zero out missing elements
X = X.*S;
I = find( X ~= 0 ); V = X(I);
fprintf(1,'Initial mtx cmplt. with zeros has min eig = %3.4f\n',min( eig(X) ));

figure(1), clf, spy(X), axis off,
%print -depsc mtx_completion_spy

%********************************************************************
% subgradient method computation
%********************************************************************
f = [+Inf]; fbest = [+Inf]; dist = [];
disp('Starting alternating projections ...')

k = 1;
MAX_ITERS = 100;

while k <= MAX_ITERS 

  % project on PSD cone
  [T lambda] = eig(X);
  ind = find( diag(lambda) < 0 );
  lambda(:,ind) = 0;
  Xproj = T*lambda*T';
  dist(end+1) = norm(Xproj - X,'fro');
  X = Xproj;

  % project on the fixed matrix values
  Xproj(I) = V;
  dist(end+1) = norm(Xproj - X,'fro');
  X = Xproj;

  k = k + 2;
  if( rem(k,100) == 0 ), fprintf(1,'iter: %d\n',k), end
end

%********************************************************************
% plot results
%********************************************************************
figure(2), clf
set(gca, 'FontSize',18);
semilogy( [1:MAX_ITERS], dist, 'b-', 'LineWidth',1.5 ), hold on,
xlabel('k');
ylabel('dist');
print -depsc mtx_completion_optdist
