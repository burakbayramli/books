x = rand(80090,1)*4-2;
y = rand(80090,1)*4-2;
z = x.*exp(-x.^2-y.^2);
tic
% Construct the interpolant
F = TriScatteredInterp(x,y,z);
toc
% Evaluate the interpolant at the locations (qx, qy), qz
%    is the corresponding value at these locations.
ti = -2:.05:2;
[qx,qy] = meshgrid(ti,ti);
tic
qz = F(qx,qy);
toc
mesh(qx,qy,qz); hold on; plot3(x,y,z,'o'); hold off
