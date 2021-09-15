% arclength_continuation.m
%
% function [x_c,Param_c,lambda_c,fnorm_c,stab_c] = ...
%    arclength_continuation(fun_name, ...
%    Param_0,Param_1,x0,AcrLenParam);
%
% This MATLAB routine uses arc-length continuation to trace
% the dependence of the solution of a nonlienar algebraic
% system upon its parameters along a single branch. It also 
% optionally computes the dynamic stability of each point.
%
% INPUT:
% fun_name = character string containing the name of
%     the routine that computes the function vector,
%     of the format
%               f = fun_name(x,theta);
% Param_0 = the starting parameter vector at lambda = 0
% Param_1 = the final parameter vector at lambda = 1
% x0 = the initial guess of the solution to use at lambda = 0
% ArcLenParam = an optional structure with solver parameters,
%      default values are assigned to each if necessary
%     .ds = the step length in arc-length used by the integrator
%     .max_iter =  max # of arc-length iterations
%     .max_corr = max # of corrector iterations
%     .atol = absolute tolerance for corrector iterations
%
% OUTPUT:
% x_c = an array in which each column vector is the
%       state vector of the system at a separate point
%       along the solution curve
% Param_c = an array in which each column vector contains
%       the parameter values at each point along the solution
%       curve
% lambda_c = a row vector containing the lambda values at
%       each point along the solution curve
% fnorm_c = a row vector containing the infinity norm of
%       the function vector at each point along the
%       solution curve (used to check for accuracy)
% stab_c = a row vector that contains a value of 1 if
%       a point is stable (or critically stable) and a 0
%       if it is unstable
%
% K.J. Beers. MIT ChE. 9/24/03. v 9/25/03.A
function [x_c,Param_c,lambda_c,fnorm_c,stab_c] = ...
    arclength_continuation(fun_name, ...
    Param_0,Param_1,x0,AcrLenParam);

% First, set the default parameters if necessary.
try
    val = ArcLenParam.ds;
catch
    norm_x = norm(x0);
    ArcLenParam.ds = sqrt(norm_x^2 + 1)/100;
end
try
    val = ArcLenParam.max_iter;
catch
    ArcLenParam.max_iter = 1000;
end
try
    val = ArcLenParam.max_corr;
catch
    ArcLenParam.max_corr = 20;
end
try
    val = ArcLenParam.atol;
catch
    ArcLenParam.atol = 1e-6;
end

% check # of output arguments to see whether
% to do stability analysis
if(nargout >= 5)
    ido_stab = 1;
else
    ido_stab = 0;
end

% extract number of functions and unknowns
N = length(x0);

% set counter on number of points identified along curve
count_pts = 0;

% First, solve system at initial steady state
Options_fsolve = optimset('LargeScale','off','Display','off', ...
    'TolFun', ArcLenParam.atol);
[x,f] = fsolve(fun_name,x0,Options_fsolve,Param_0);
% Compute Jacobian of function from finite differences
Jac = ac_calc_Jac(fun_name,x,Param_0,f);
% store result at first point along solution curve
x_c(:,count_pts+1) = x;
% parameter space
lambda = 0;
Param = (1-lambda)*Param_0 + lambda*Param_1;
lambda_c(:,count_pts+1) = lambda;
Param_c(:,count_pts+1) = Param;
fnorm_c(:,count_pts+1) = norm(f,inf);
if(ido_stab)
    stab_c(count_pts+1) = ac_calc_stab(Jac);
end
count_pts = count_pts + 1;  % increment counter
x_old = x;  lambda_old = lambda;

% generate random row vector for normalization
c_rand = rand(1,N+1);

% Now, perform iterations along arclength continuation
icomplete_arclength = 0;
for iter=1:ArcLenParam.max_iter
    
    % check to see if lambda exceeds 1, so that
    % we can stop
    if(lambda >= 1)
        icomplete_arclength = 1;
        break;
    end
   
   % compute partial of function with respect to
   % lambda from finite differences
   df_dlambda = ac_calc_df_dlambda(fun_name,x,lambda,Param_0,Param_1,f);
   
   % construct matrix for solving dz/ds
   A = zeros(N+1,N+1);
   A(1:N,1:N) = Jac;
   A(1:N,N+1) = df_dlambda;
   A(N+1,1:N+1) = c_rand;
   
   % solve for dz/ds
   b = zeros(N+1,1); b(N+1) = 1;
   v = A\b;
   dz_ds = v / norm(v,2);
   
   % make sure that we move in same direction along curve
   % as previously
   dz_old = zeros(N+1,1);
   dz_old(1:N) = x - x_old;
   dz_old(N+1) = lambda - lambda_old;
   if(dot(dz_ds,dz_old) < 0)
       dz_ds = -dz_ds;
   end
   
   % Now, use explicit Euler predictor to find predict new value
   % along curve
   x_old = x;  lambda_old = lambda;
   x = x + dz_ds(1:N)*ArcLenParam.ds;
   lambda = lambda + dz_ds(N+1)*ArcLenParam.ds;
   Param = (1-lambda)*Param_0 + lambda*Param_1;
   f = feval(fun_name,x,Param);
   % Now, perform corrector iterations
   icorr_OK = 0;
   % form fixed augmented Jacobian maitrx by replacing
   % last row of A with dz_ds
   A(N+1,:) = dz_ds';
   % as this matrix is fixed for each corrector iteration,
   % perform LU decomposition
   [L,U,P] = lu(A);
   for k_corr = 1:ArcLenParam.max_corr
       % check for convergence
       if(norm(f,2) <= ArcLenParam.atol)
           icorr_OK = 1;
           break;
       end
       % get delta_z
       v = L\(P*[-f;0]);
       delta_z= U\v;
       x = x + delta_z(1:N);
       lambda = lambda + delta_z(N+1);
       % get new function value
       Param = (1-lambda)*Param_0 + lambda*Param_1;
       f = feval(fun_name,x,Param);
   end
   if(icorr_OK==0)
       error('arclength_continuation: Corrector iterations failed');
   end
   % Compute new Jacobian of function from finite differences
   Jac = ac_calc_Jac(fun_name,x,Param,f);   

   % store results
   x_c(:,count_pts+1) = x;
   lambda_c(:,count_pts+1) = lambda;
   Param_c(:,count_pts+1) = Param;
   fnorm_c(:,count_pts+1) = norm(f,inf);
   % if desired, compute the stability of the steady state
   % and set stab_c to 1 if stable, else 0
   if(ido_stab)
       stab_c(count_pts+1) = ac_calc_stab(Jac);
   end
   count_pts = count_pts + 1;  % increment counter

end

return;


%--------------------------------------------------------------
% This routine uses finite differences to evaluate the Jacobian
% matrix of the function.
function Jac = ac_calc_Jac(fun_name,x,theta,f);

N = length(x);  Jac = zeros(N,N);
epsilon = sqrt(eps);
for k=1:N
    x_new = x;  x_new(k) = x(k) + epsilon;
    f_new = feval(fun_name,x_new,theta);
    Jac(:,k) = (f_new-f)/epsilon;
end

return;


%--------------------------------------------------------------
% This routine uses finite differences to evaluate the partials
% of the function with respect to lambda.
function df_dlambda = ...
    ac_calc_df_dlambda(fun_name,x,lambda,Param_0,Param_1,f);

epsilon = sqrt(eps);
lambda_new = lambda + epsilon;
Param_new = (1-lambda_new)*Param_0 + lambda_new*Param_1;
f_new = feval(fun_name,x,Param_new);
df_dlambda = (f_new-f)/epsilon;

return;


%----------------------------------------------------------------
% This function computes the stability of the steady state from the
% Jacobian and returns a one if it it stable (or critically stable),
% else a zero if it unstable.
function istable = ac_calc_stab(Jac);

% First compute real parts of eigenvalues of the Jacobian
eig_vals_real = real(eig(Jac));

% Then, check if any have real parts greater than zero
iunstable = length(find(eig_vals_real > 0));

if(iunstable)
    istable = 0;
else
    istable = 1;
end

return;
