% generate a sparse random matrix of given density
%m=10; n=100;
m=3; n=4;
%density=0.2;
#A = sprandn(m,n,density);
A = [-1 -5 0 0 ;
      1  1 1 0 ;
      1  3 0 1 ]

A = sparse(A);

% choose feasible x, y, s at random, with x and s each about half-full
xfeas = [rand(n/2,1); zeros(n-(n/2),1)];
sfeas = [zeros(n/2,1); rand(n-(n/2),1)];
xfeas = xfeas(randperm(n)); sfeas = sfeas(randperm(n)); 
yfeas = (rand(m,1)-0.5)*4;

% choose b and p to make this (x,y,s) feasible
b = A*xfeas; p=A'*yfeas+sfeas;

% call the solver
%[x,y,s,f] = pathfollow(A,b,p);
[x,y,s,f] = pdip(A,b,p);

fprintf(1,' final primal value: %12.6e \n', p'*x);
fprintf(1,' final dual   value: %12.6e \n', b'*y);
fprintf(1,' primal infeas: %12.6e \n', norm(A*x-b));
fprintf(1,' dual   infeas: %12.6e \n', norm(A'*y+s-p));

x
