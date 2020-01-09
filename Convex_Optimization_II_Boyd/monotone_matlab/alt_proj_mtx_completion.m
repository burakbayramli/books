% Positive semidefinite matrix completion
%   find        X
%   subject to  X(i,j) = val,   where (i,j,val) is the data 
%               X is PSD mtx with size n-by-n
% via alternating projections method and Dykstra's algorithm.
%
% This script runs both alternating projections and Dykstra's algorithm for
% comparison.
%
% EE364b Convex Optimization II, S. Boyd
% Written by Almir Mutapcic, 01/19/07 and Eric Chu, 04/25/11
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
print -depsc mtx_completion_dr_spy

%********************************************************************
% subgradient method computation
%********************************************************************
dist = [];
disp('Starting alternating projections ...')

k = 1;
MAX_ITERS = 100;

while k <= MAX_ITERS 
  Xprev = X;
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
% douglas rachford alternating projection algorithm
%********************************************************************
dr_dist = [];
disp('Starting DR alternating projections ...')

k = 1;
MAX_ITERS = 100;

% initial state
Z = zeros(n);
while k <= MAX_ITERS 

  % x^{k+1/2}: project on PSD cone
  [T lambda] = eig(Z);
  ind = find( diag(lambda) < 0 );
  lambda(:,ind) = 0;
  X_half = T*lambda*T';
  dr_dist(end+1) = norm(X_half - X,'fro');
  
  % z^{k+1/2}
  Z_half = 2*X_half - Z;


  % x^{k+1}: project on the fixed matrix values
  X = Z_half;
  X(I) = V;
  dr_dist(end+1) = norm(X_half - X,'fro');
  
  % z^{k+1}
  Z = Z + X - X_half;

  
  k = k + 2;
  if( rem(k,100) == 0 ), fprintf(1,'iter: %d\n',k), end
end

%********************************************************************
% plot results
%********************************************************************
figure(2), clf
set(gca, 'FontSize',18);
semilogy( 1:MAX_ITERS, dist, 'b-', 1:MAX_ITERS, dr_dist, 'r-', 'LineWidth',1.5 ), hold on,
xlabel('k');
ylabel('dist');
axis([1 100 10^-5 10^3])
print -depsc mtx_completion_dr