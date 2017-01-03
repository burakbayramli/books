function demoDecisionBoundaries3
% Decision boundaries induced by a mixture of three 2D Gaussians
% Based on code by Tommi Jaakkola

figure(2); clf

% All decision boundaries are linear
subplot(2,2,1)
p1 = 1/3; p2 = 1/3; p3 = 1/3;
mu1 = [0 0]'; mu2 = [0 5]'; mu3 = [5 5]';
S1 = eye(2); S2 = eye(2); S3 = eye(2); 
plotgaussians3(p1, mu1, S1, p2, mu2, S2, p3, mu3, S3);
title('All boundaries are linear')

% Some decision boundaries are linear, some quadratic
subplot(2,2,2)
p1 = 1/3; p2 = 1/3; p3 = 1/3;
mu1 = [0 0]'; mu2 = [0 5]'; mu3 = [5 5]';
S1 = [4 0; 0 1]; S2 = eye(2); S3 = eye(2); 
plotgaussians3(p1, mu1, S1, p2, mu2, S2, p3, mu3, S3);
title('Some linear, some quadratic')

% All decision boundaries are quadratic
subplot(2,2,3)
p1 = 1/3; p2 = 1/3; p3 = 1/3;
mu1 = [0 0]'; mu2 = [0 5]'; mu3 = [5 5]';
S1 = [1 0; 0 3]; S2 = [4 0; 0 1]; S3 = [1 0; 0 2]; 
plotgaussians3(p1, mu1, S1, p2, mu2, S2, p3, mu3, S3);
title('All boundaries are quadratic')

% There are only 2 decision regions
subplot(2,2,4)
p1 = 0; p2 = 0.5; p3 = 0.5;
mu1 = [0 0]'; mu2 = [0 5]'; mu3 = [5 5]';
S1 = [1 0; 0 3]; S2 = [4 0; 0 1]; S3 = [1 0; 0 2]; 
plotgaussians3(p1, mu1, S1, p2, mu2, S2, p3, mu3, S3);
title('There are only 2 decision regions')

%%%%%%%%%%%%

function h = plotgaussians3(p1,mu1,S1,p2,mu2,S2,p3,mu3,S3)

[x,y] = meshgrid(linspace(-10,10,100), linspace(-10,10,100));
[m,n]=size(x);
X = [reshape(x, n*m, 1) reshape(y, n*m, 1)];
g1 = reshape(mvnpdf(X, mu1(:)', S1), [m n]);
g2 = reshape(mvnpdf(X, mu2(:)', S2), [m n]);
g3 = reshape(mvnpdf(X, mu3(:)', S3), [m n]);
hold;
contour(x,y,g1, 'r:');
contour(x,y,g2, 'b--');
contour(x,y,g3, 'g-.');
% decision boundaries
[cc,hh]=contour(x,y,g1*p1-max(g2*p2, g3*p3),[0 0],'-k');  set(hh,'linewidth',3);
[cc,hh]=contour(x,y,g2*p2-max(g1*p1, g3*p3),[0 0],'-k');  set(hh,'linewidth',3);
[cc,hh]=contour(x,y,g3*p3-max(g2*p2, g1*p1),[0 0],'-k');  set(hh,'linewidth',3);
axis equal
