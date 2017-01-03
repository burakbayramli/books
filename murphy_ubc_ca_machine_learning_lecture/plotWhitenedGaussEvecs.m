mu = [1 0]';        % mean (must be row vector for mvnpdf)
S  = [4 3; 3 4];    % covariance
[U,D] = eig(S);      % U = eigenvectors, D= diagonal matrix of eigenvalues.

% Evaluate p(x) on a grid.
stepSize = 0.5;
[x,y] = meshgrid(-5:stepSize:5,-5:stepSize:5); % Create grid.
[r,c]=size(x);

% data(k,:) = [x(k) y(k)] for pixel k
data = [x(:) y(:)];
p = mvnpdf(data, mu', S);
p = reshape(p, r, c);

% slow method
p2 = zeros(size(x));
for i=1:r
  for j=1:c
    p2(i,j) = mvnpdf([x(i,j) y(i,j)], mu', S);
  end;
end;  
assert(approxeq(p, p2))

% scale density so it sums to 1 
p=p*stepSize^2;  %  p2(x,y)  defeq  p(x: x+dx, y: y+ dy) approx p(x,y) dx dy
assert(approxeq(sum(p(:)), 1, 1e-1))

% Plot p(x)
figure(1)
clf;
subplot(221)
surfc(x,y,p);                  % 3D plot
view(-10,50);
xlabel('x','fontsize',15);
ylabel('y','fontsize',15);
zlabel('p(x,y)','fontsize',15);
subplot(222)
contour(x,y,p);           % Plot contours
axis('square');           % Set axes range.
xlabel('x','fontsize',15);
ylabel('y','fontsize',15);
% Plot first eigenvector
line([mu(1) mu(1)+sqrt(D(1,1))*U(1,1)],[mu(2) mu(2)+sqrt(D(1,1))*U(2,1)],'linewidth',3)
% Plot second eigenvector
line([mu(1) mu(1)+sqrt(D(2,2))*U(1,2)],[mu(2) mu(2)+sqrt(D(2,2))*U(2,2)],'linewidth',3)


% Compute whitening transform:
A = sqrt(inv(D))*U';
mu2 = A*mu;
S2  = A*S*A';

p2 = reshape(mvnpdf(data, mu2', S2), r, c);
% scale density so it sums to 1 
p2=p2*stepSize^2;  % p(dx,dy) = p(x,y) dx dy
assert(approxeq(sum(p2(:)), 1, 1e-1))


subplot(223)
surfc(x,y,p2);                  % 3D plot    
view(-10,50);
xlabel('x','fontsize',15);
ylabel('y','fontsize',15);
zlabel('p2(x,y)','fontsize',15);
subplot(224)
contour(x,y,p2);          % Plot contours
axis('square');           % Set axes range.
xlabel('x','fontsize',15);
ylabel('y','fontsize',15);
[U2,D2] = eig(S2) % compute eigenvalues and eigenvectors
% Plot first eigenvector
line([mu2(1) mu2(1)+sqrt(D2(1,1))*U2(1,1)],[mu2(2) mu2(2)+sqrt(D2(2,2))*U2(2,1)],'linewidth',3)
% Plot second eigenvector
line([mu2(1) mu2(1)+sqrt(D2(2,2))*U2(1,2)],[mu2(2) mu2(2)+sqrt(D2(2,2))*U2(2,2)],'linewidth',3)
axis('square')



