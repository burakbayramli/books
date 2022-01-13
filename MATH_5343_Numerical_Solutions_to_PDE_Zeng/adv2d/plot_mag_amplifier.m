mu1 = linspace(-1,1,201);
mu2 = linspace(-1,1,201);
[MU1, MU2] = meshgrid(mu1,mu2);

magA = zeros(size(MU1));
theta1 = linspace(0,2*pi,61);
theta2 = linspace(0,2*pi,61);
for i1 = 1:length(theta1)
  th1 = theta1(i1);
  for i2 = 1:length(theta2)
    th2 = theta2(i2);
    A = abs(1-MU1*(1-cos(th1)+i*sin(th1))+MU2*(cos(th2)+i*sin(th2)-1));
    magA = max(magA, A);
  end
  [M, ct] = contour(MU1,MU2,magA,[1.0 1.01 1.05 1.1 1.2 1.4 1.6 1.8 2.0],'ShowText','On');
  set(ct, 'LineWidth', 3);
  axis equal;
end
