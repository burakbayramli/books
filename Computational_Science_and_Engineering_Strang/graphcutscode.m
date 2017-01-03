%2.9  graphcutscode.m

N = 10; W = zeros(2*N,2*N) ;           % Generate 2N nodes in two clusters
rand('state',100)                      % rand repeats to give the same graph
for i = 1:2*N-1
  for j = i+1:2*N
    p = 0.7 - 0.6*mod(j-i,2);          % p = 0.1 when j - i is odd, 0.7 else
    W(i,j) = rand < p;                 % Insert edges with probability p
  end                                  % The weights are w_{ij} = 1 (or zero)
end                                    % So far W is strictly upper triangular
W = W + W'; D = diag(sum(W));          % Adjacency matrix W, degrees in D
G = D - W; [V,E] = eig(G,D);           % Eigenvalues of Gx = lambda Dx in E
[a,b] = sort(diag(E)); z = V (:,b(2)); % Fiedler eigenvector z for lambda_2
plot(sort(z),'.-');                    % Show groups of Fiedler components
theta = [1:N]*2*pi/N; x = zeros(2*N,1); y = x ;         % Angles to plot graph
x(1:2:2*N-1) = cos(theta)-1; x(2:2:2*N) = cos(theta)+1;
y(1:2:2*N-1) = sin(theta)-1; y(2:2:2*N) = sin(theta)+1;
subplot(2,2,1), gplot(W,[x,y]), title('Graph')          % First of four plots
subplot(2,2,2), spy(W), title('Adjacency matrix W')     % Clusters unclear in W
subplot(2,2,3), plot(z(1:2:2*N-1),'ko'), hold on        % z separates clusters
plot(z(2:2:2*N),'r*'), hold off, title('Fiedler components')
[c,d] = sort(z); subplot(2,2,4), spy(W(d,d)), title('Reordered Matrix W')
