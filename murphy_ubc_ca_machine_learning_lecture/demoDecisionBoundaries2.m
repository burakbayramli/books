function demoDecisionBoundaries2
% Decision boundaries induced by a mixture of two 2D Gaussians
% Based on code by Tommi Jaakkola

figure(1);clf
hold on

% linear decision boundary, with means on opposite sides
subplot(3,2,1)
p1 = 0.5; p2 = 1-p1;
mu1 = [1 1]'; mu2 = [-1 -1]';
S1 = eye(2); S2 = eye(2);
plotgaussians2(p1, mu1, S1, p2, mu2, S2);
title('linear boundary')

  
% linear decision boundary, where both means are on the same side
subplot(3,2,2)
p1 = 0.995; p2 = 1-p1;
mu1 = [1 1]'; mu2 = [-1 -1]';
S1 = eye(2); S2 = eye(2);
plotgaussians2(p1, mu1, S1, p2, mu2, S2);
title('linear boundary, both means on same side')

% parabolic decision boundary
subplot(3,2,3)
p1 = 0.5; p2 = 1-p1;
mu1 = [1 1]'; mu2 = [-1 -1]';
S1 = [2 0; 0 1]; S2 = eye(2);
plotgaussians2(p1, mu1, S1, p2, mu2, S2);
title('parabolic boundary')

% one of the classes is represented by 2 disconnected regions
subplot(3,2,4)
p1 = 0.5; p2 = 1-p1;
mu1 = [0 0]'; mu2 = [0 0]';
S1 = [4 0; 0 2]; S2 = [1 0; 0 2];
plotgaussians2(p1, mu1, S1, p2, mu2, S2);
title('disconnected regions')

% circular decision boundary
subplot(3,2,5)
p1 = 0.5; p2 = 1-p1;
mu1 = [0 0]'; mu2 = [0 0]';
S1 = [1 0; 0 1]; S2 = [2 0; 0 2];
plotgaussians2(p1, mu1, S1, p2, mu2, S2);
title('circular boundary')

% skewed ellipsoid decision boundary, with only one mean inside
subplot(3,2,6)
p1 = 0.5; p2 = 1-p1;
mu1 = [-2 1]'; mu2 = [1 1]';
S1 = [1 0; 0 0.5]; S2 = [2 0; 0 4];
plotgaussians2(p1, mu1, S1, p2, mu2, S2);
title('skewed elliptical boundary')

%%%%%%%%%%
  
function h = plotgaussians2(p1,mu1,S1,p2,mu2,S2)

[x,y] = meshgrid(linspace(-10,10,100), linspace(-10,10,100));
[m,n]=size(x);
X = [reshape(x, n*m, 1) reshape(y, n*m, 1)];
g1 = reshape(mvnpdf(X, mu1(:)', S1), [m n]);
g2 = reshape(mvnpdf(X, mu2(:)', S2), [m n]);
hold;
contour(x,y,g1, 'r:');
contour(x,y,g2, 'b--');
[cc,hh]=contour(x,y,p1*g1-p2*g2,[0 0], '-k');
set(hh,'linewidth',3);
axis equal


