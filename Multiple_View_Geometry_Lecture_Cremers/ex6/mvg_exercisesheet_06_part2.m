function mvg_exerciseSheet_06_part2

% Read input images:
image1 = double(imread('batinria0.tif'));
image2 = double(imread('batinria1.tif'));

% Number of corresponding point pairs:
nPoints = 12;%8;   % or 12 e.g.

% Get corresponding point pairs:
%[x1,y1,x2,y2] = getpoints(image1,image2,nPoints);
[x1,y1,x2,y2] = getpoints2();      % use predefined points (see below)

% Intrinsic camera paramter matrices
K1 = [844.310547 0 243.413315; 0 1202.508301 281.529236; 0 0 1];
K2 = [852.721008 0 252.021805; 0 1215.657349 288.587189; 0 0 1];
K1 = inv(K1);
K2 = inv(K2);

% Transform image coordinates with inverse camera matrices:
% can be done without for-loop as well
for i = 1:nPoints
   l = K1 * [x1(i); y1(i); 1];
   r = K2 * [x2(i); y2(i); 1];
   x1(i) = l(1);
   y1(i) = l(2);
   x2(i) = r(1);
   y2(i) = r(2);
end

% Compute constraint matrix chi:
chi = zeros(nPoints,9);
for i = 1:nPoints
    chi(i,:) = kron([x1(i) y1(i) 1],[x2(i) y2(i) 1])';
end
rank_chi = rank(chi)

% Find minimizer for |chi*E|:
[UChi,DChi,VChi] = svd(chi);

% Unstacked ninth column of V:
E = reshape(VChi(:,9),3,3)

% SVD of E
[U,D,V] = svd(E);
if det(U) < 0 || det(V) < 0
    [U,D,V] = svd(-E);
end

% Project E onto essential space (replace eigenvalues):
D(1,1) = 1;
D(2,2) = 1;
D(3,3) = 0;

% Final essential matrix:
E = U * D * V';

% Recover R and T from the essential matrix E:
% (Compare Slides)
Rz1 = [0 1 0; -1 0 0; 0 0 1]';
Rz2 = [0 -1 0; 1 0 0; 0 0 1]';
R1 = U * Rz1' * V';
R2 = U * Rz2' * V';
T_hat1 = U * Rz1 * D * U';
T_hat2 = U * Rz2 * D * U';

% Translation belonging to T_hat
T1 = [ -T_hat1(2,3); T_hat1(1,3); -T_hat1(1,2) ];
T2 = [ -T_hat2(2,3); T_hat2(1,3); -T_hat2(1,2) ];


% Compute scene reconstruction and correct combination of R and T:
reconstruction(R1,T1,x1,y1,x2,y2,nPoints);
reconstruction(R1,T2,x1,y1,x2,y2,nPoints);
reconstruction(R2,T1,x1,y1,x2,y2,nPoints);
reconstruction(R2,T2,x1,y1,x2,y2,nPoints);

end



% Compute correct combination of R and T and reconstruction of 3D points
function reconstruction(R,T,x1,y1,x2,y2,nPoints)

% Structure reconstruction matrix M:
M = zeros(3*nPoints, nPoints + 1);
for i = 1:nPoints
   x2_hat = hat([x2(i) y2(i) 1]);
   
   M(3*i - 2  :  3*i, i)           = x2_hat * R * [x1(i); y1(i); 1];
   M(3*i - 2  :  3*i, nPoints + 1) = x2_hat * T;
end

% Get depth values (eigenvector to the smallest eigenvalue of M'M):
[V,D] = eig(M' * M);
lambda = V(1:nPoints, 1);
gamma  = V(nPoints + 1, 1);

% Determine correct combination of R and T:
if lambda >= zeros(nPoints,1)
    display(R);
    display(T);
    display(lambda);
    display(gamma);
    
    % Visualize the 3D points:
    figure, plot3(x1,y1,lambda,'+');
end

end





% ================
% Hat-function
function A = hat(v)
    A = [0 -v(3) v(2) ; v(3) 0 -v(1) ; -v(2) v(1) 0];
end



% ================
% function getpoints
function [x1,y1,x2,y2] = getpoints(image1,image2,nPoints)

x1 = zeros(nPoints,1);
y1 = zeros(nPoints,1);
x2 = zeros(nPoints,1);
y2 = zeros(nPoints,1);

% Click points in image1:
% Can be done without for-loop: ginput(nPoints)
figure; imshow(uint8(image1));
hold on;
for i = 1:nPoints
    [x,y] = ginput(1);
    x1(i) = double(x);
    y1(i) = double(y);
    plot(x, y, 'r+');
end
hold off;


% Click points in image2:
figure; imshow(uint8(image2));
hold on;
for i = 1:nPoints
    [x,y] = ginput(1);
    x2(i) = double(x);
    y2(i) = double(y);
    plot(x, y, 'r+');
end
hold off;

end



% ================
% function getpoints2  --> points already defined
function [x1,y1,x2,y2] = getpoints2()

x1 = [
   10.0000
   92.0000
    8.0000
   92.0000
  289.0000
  354.0000
  289.0000
  353.0000
   69.0000
  294.0000
   44.0000
  336.0000
  ];

y1 = [ 
  232.0000
  230.0000
  334.0000
  333.0000
  230.0000
  278.0000
  340.0000
  332.0000
   90.0000
  149.0000
  475.0000
  433.0000
    ];
 
x2 = [
  123.0000
  203.0000
  123.0000
  202.0000
  397.0000
  472.0000
  398.0000
  472.0000
  182.0000
  401.0000
  148.0000
  447.0000
    ];

y2 = [ 
  239.0000
  237.0000
  338.0000
  338.0000
  236.0000
  286.0000
  348.0000
  341.0000
   99.0000
  153.0000
  471.0000
  445.0000
    ];

end
