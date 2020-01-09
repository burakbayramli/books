% Signal reconstruction example
% reconstruct a spike signal using l1 and l2 regularization
%
% EE364b Convex Optimization II, S. Boyd
% Written by Kwangmoo Koh, 03/09/07

rand('state',1); randn('state',1);

m = 200;        % number of observation
n = 1000;       % signal length
s = 30;         % number of spikes in signal

% generate a spike signal
x0 = zeros(n,1);
q  = randperm(n);
x0(q(1:s)) = sign(randn(s,1));

% generate a random measurement matrix
A = randn(m,n);

% generate a noisy measurement
sigma = 0.1;
v = sigma*randn(m,1);
y = A*x0 + v;

gamma = 0.001;

% reconstruct signal using l1 regularization
cvx_begin
variable x1(n);
minimize( norm(y-A*x1,2)+gamma*norm(x1,1) )
cvx_end

% reconstruct signal using l2 regularization
cvx_begin
variable x2(n);
minimize( norm(y-A*x2,2)+gamma*norm(x2,2) )
cvx_end

% plot signals
figure; bar(x0); axis([1 n -1.2 1.2]);
print -deps spike_original.eps
figure; bar(x1); axis([1 n -1.2 1.2]);
print -deps spike_l1.eps
figure; bar(x2); axis([1 n -1.2 1.2]);
print -deps spike_l2.eps
