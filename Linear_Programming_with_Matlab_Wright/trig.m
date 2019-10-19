
% set starting point and input parameters
z0=[1; 3; 0]; eps = 1.e-12; itmax = 10;

% call newton function
z = newton(z0,@Ftrig,@Jtrig,eps,itmax);

% output results
fprintf(' final z=[%9.4e, %9.4e, %9.4e]\n', z);
fprintf(' final norm of F is %9.4e\n', norm(Ftrig(z)));

