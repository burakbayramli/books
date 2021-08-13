function poissoncontrol_errortest(sol_y,sol_u,xy,beta)
%POISSONCONTROL_ERRORTEST computes error estimates for Problem 3 sol'n
%(known) exact solution of Problem 3 
%   poissoncontrol_errortest(sol_y,sol_u,xy,np,beta)
%   input
%          sol_y        computed solution of the state y
%          sol_u        computed solution of the control u
%          xy           matrix detailing node points
%          beta         regularization parameter of the problem
%   
%   IFISS function: JWP; 27 June 2012.  DJS; 29 August 2012
% Copyright (c) 2012 J.W. Pearson

% Compute the exact solution of state and control at each node
y_exact = 1/(1+4*beta*pi^4)*sin(pi*xy(:,1)).*sin(pi*xy(:,2));
u_exact = 2*pi^2*y_exact;

% Estimate L_2 and errors by comparing exact and computed solutions
L2_y = norm(y_exact-sol_y)/norm(y_exact); % L_2, state
L2_u = norm(u_exact-sol_u)/norm(u_exact); % L_2, control

% Provide output of error estimates
fprintf('L_2 error estimate for state variable is %10.4e \n',L2_y)
fprintf('L_2 error estimate for control variable is %10.4e \n',L2_u)
