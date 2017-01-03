
N = 1000;
xs = unifrnd(-1,1,1,N);
ys = unifrnd(-1,1,1,N);
D1 = [xs; ys];

figure(1);clf
plot(D1(1,:), D1(2,:),'.')
axis([-2 2 -2 2])
print(gcf, 'C:\kmurphy\Teaching\stat406-spring06\Book\figures\secondOrderSquare.eps','-deps');

th = 45*(2*pi/360);
A = [cos(th) -sin(th); sin(th) cos(th)];
D2 = A*D1;

figure(2);clf
plot(D2(1,:),D2(2,:),'.')
axis([-2 2 -2 2])
print(gcf, 'C:\kmurphy\Teaching\stat406-spring06\Book\figures\secondOrderSquareRotated.eps','-deps');

C1=cov(D1')
C2=cov(D2')

