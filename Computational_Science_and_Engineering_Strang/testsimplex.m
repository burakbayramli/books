A = [1, 1; 16, 8; 9000, 5000];
b = [44, 512, 300000]';
c = [30000, 20000]';
basis = [2];
[x,y,cost] = simplex(A,b,c,basis);
disp('x');
disp(x);
disp('y');
disp(y);
disp('cost');
disp(cost);

