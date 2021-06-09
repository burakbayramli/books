N = 10;
[x,y] = meshgrid(linspace(-1,1,N),linspace(-1,1,N));
size(x);
xx = x(:);
yy = y(:);
A = zeros(2,N*N);
A(1,:)=xx;
A(2,:)=yy;
A
