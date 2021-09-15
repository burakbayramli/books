% search_bifurcation.m
% function [x_b,theta_b,lambda_b,iflag] = search_bifurcation(fun_name, ...
%    theta_0,theta_1,a,b,x0,num_lambda);
%
% This MATLAB routine searches for a bifurcation point
% along a user defined path in parameter space, where
% theta varies as a function of an order parameter lambda.
% The inputs to the program are:
%  fun_name = a character string containing the name of the function
%  theta_0 = the value of the parameter vector at lambda = 0
%  theta_1 = the value of the parameter vector at lambda = 1
%  a = the initial lambda value at which one starts as an initial guess
%  b = the final lambda value that one uses as an initial guess
%  x0 = the initial guess of the solution
%  num_lambda = the number of lambda points to try as an initial guess
%
% The routine then attempts to search for a bifurcation point along
% the line using values of the initial guesses of lambda between
% a and b until a bifurcation point is found. The MATLAB routine
% fsolve() is invoked to solve the augmented system.
%
% The program output is:
%  x_b = the solution at the bifurcation point
%  theta_b = the bifurcation point parameter vector
%  lambda_b = the lambda value at which the bifurcation point
%             was found
%
% K. J. Beers. MIT ChE. 9/23/03

function [x_b,theta_b,lambda_b,ifound_b] = search_bifurcation(fun_name, ...
    theta_0,theta_1,a,b,x0,num_lambda, ...
    max_Newton_iter,atol,max_line);

% extract number of unknowns in system
N = length(x0);  Naug = N+1;

% generate the vector of lambda initial guesses
lambda_guess_vect = linspace(a,b,num_lambda);

% now, iterate with different guesses of lambda until a
% bifurcation point is found
ifound_b = 0;
for k=1:num_lambda
    lambda = lambda_guess_vect(k);
    x = x0;
    % get parameter vector
    theta = (1-lambda)*theta_0 + lambda*theta_1;
    % call fsolve with augmented set of equations
    Options = optimset('LargeScale','off','Display','off');
    z0 = [x0; lambda];
    [z_b,faug_b,exitflag] = fsolve(@sb_calc_f_aug, ...
        z0,Options,fun_name,theta_0,theta_1);
    x_b = z_b(1:N);  lambda_b = z_b(N+1);
    if(exitflag > 0)  % if sucessful
        ifound_b = 1;
        theta_b = (1-lambda_b)*theta_0 + lambda_b*theta_1;
        return;
    end        
end

% if did not find a bifurcation point
ifound_b = 0;
x_b = x; theta_b = theta; lambda_b = lambda;
return;


%-------------------------------------------------------------
% This routine computes the augmented function vector that is
% to be zero at the bifurcation point.
function f_aug = sb_calc_f_aug(z,fun_name,theta_0,theta_1);

N = length(z) - 1;  x = z(1:N);  lambda = z(N+1);
theta = (1-lambda)*theta_0 + lambda*theta_1;

f_aug = zeros(N+1,1);
f_aug(1:N) = feval(fun_name,x,theta);

% now, estimate Jacobian from finite differences and
% compute determinant
Jac = sb_calc_Jac(fun_name,x,theta,f_aug(1:N));
det_Jac = det(Jac);
f_aug(N+1) = det_Jac;

return;


%--------------------------------------------------------------
% This routine uses finite differences to evaluate the Jacobian
% matrix of the function.
function Jac = sb_calc_Jac(fun_name,x,theta,f);

N = length(x);  Jac = zeros(N,N);
epsilon = sqrt(eps);
for k=1:N
    x_new = x;  x_new(k) = x(k) + epsilon;
    f_new = feval(fun_name,x_new,theta);
    Jac(:,k) = (f_new-f)/epsilon;
end

return;
