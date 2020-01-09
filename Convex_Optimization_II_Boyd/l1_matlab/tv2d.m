% Total variation reconstruction example in 2d
% actually on general graph
%
% EE364b Convex Optimization II, S. Boyd
% Written by Kwangmoo Koh, 02/27/07


N = 31;                 % graph of N x N grid
n = N^2;                % number of vertices
m = floor(n/8);         % number of samples

% generate an incidence matrix A of graph
A = zeros(2*N*(N-1),n); % matrix of size (edges x vertices)
e = 1;
for v = 1:n
    if (mod(v,N) ~= 0)      % if not on the bottom boundary
        A(e,v)   = A(e,v)   + 1;
        A(e,v+1) = A(e,v+1) - 1;
        e = e + 1;
    end
    if (ceil(v/N) <= N-1)   % if not on the right boundary
        A(e,v)   = A(e,v)   + 1;
        A(e,v+N) = A(e,v+N) - 1; 
        e = e + 1;
    end
end

% generate a true signal (node value) X in 2d
% construct X to be 1 only inside the cricle centered at (center,center)
% with radius R, otherwise 0.
R = 10;                 % radius
X = zeros(N,N);
center = floor(N+1)/2;  % center value
for i = 1:N
    for j = 1:N
        if ((i-center)^2+(j-center)^2 <= R^2)
            X(i,j) = 1;
        end
    end
end

% generate a (random) measurement
F = randn(m,n);
y = F*X(:);

% reconstruct signal using l1 regularization
cvx_begin
variable x1(n)
minimize(norm(A*x1,1))
subject to
    F*x1==y;
cvx_end

% reconstruct signal using l2 regularization
cvx_begin
variable x2(n)
minimize(norm(A*x2,2))
subject to
    F*x2==y;
cvx_end

% plot signals
figure(1);
bar3(X);
zlim([-0.5 1.5]);
title('original');
print -depsc tv2d_original.eps

figure(2);
bar3(reshape(x1,N,N));
zlim([-0.5 1.5]);
title('l1-norm');
print -depsc tv2d_l1norm.eps

figure(3);
bar3(reshape(x2,N,N));
zlim([-0.5 1.5]);
title('l2-norm');
print -depsc tv2d_l2norm.eps


