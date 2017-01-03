rand('state',0);

global m n T B Cb Cr Cd lambda clip
m = 5;
n = 3;
T = 1000;
B = 5;
Cb = 1;
Cr = 1;
Cd = 10;
lambda = 0.1 * rand(m,n);
idx = (rand(m,n) > 0.9);
lambda(1,1) = lambda(1,1) + 0.4;
lambda(2,2) = lambda(2,2) + 0.4;
lambda(3,3) = lambda(3,3) + 0.4;
clip = @(x,a,b) min(max(a,x),b);

