% gradient_minimizer.m
%
% [x,F,g,iflag,x_traj] = ...
%       gradient_minimizer(func_name,x0,OptParam,ModelParam);
%
% This MATLAB routine employs either the steepest descent or conjugate
% gradient (Polak-Ribiere) method to minimize a cost function.
% The input parameters are :
%    func_name = character string naming the routine that returns the
%                cost function and the gradient
%    x0 = an initial guess of the parameter vector
%    OptParam = a structure containing the optimization method parameters,
%         .method = 0(steepest descent), 1(PR-CG), 2(mixed CG/SD)
%         .abs_tol, tolerance value on 2-norm of gradient (default 1e-6)
%         .iter_max = max # of gradient iterations (default 10*length(x0))
%         .dx_line_max = max change in x component during line search
%               (default = max(0.1 * max(abs(x0)),1e-5) )
%         .iter_line_max = max # of weak line search iterations
%               (default 1000)
%         .sigma = scalar for rate of descent condition (default 1e-5)
%    ModelParam = a structure of parameters that may be passed to the
%            cost function routine
%
% K. Beers. MIT ChE. 7/31/03

function [x,F,g,iflag,x_traj] = ...
    gradient_minimizer(func_name,x0,OptParam,ModelParam);

iflag = 0;

% set OptParam to default values if necessary
% OptParam.method
try
    val = OptParam.method;
catch
    OptParam.method =  1;
end
% OptParam.abs_tol
try
    val = OptParam.abs_tol;
catch
    OptParam.abs_tol = 1e-6;
end
% OptParam.iter_max
try
    val = OptParam.iter_max;
catch
    OptParam.iter_max = 10*length(x0);
end
% OptParam.dx_line_max
try
    val = OptParam.dx_line_max;
catch
    OptParam.dx_line_max = max(0.1*max(abs(x0)),1e-5);
end
% OptParam.iter_line_max
try
    val = OptParam.iter_line_max;
catch
    OptParam.iter_line_max = 1000;
end
try
    val = OptParam.sigma;
catch
    OptParam.sigma = 1e-5;
end


% set estimate at initial guess
x = x0;

% compute cost function and gradient
[F,g,iOK] = feval(func_name,x,ModelParam);
if(~iOK)
    error('failed cost function/gradient evaluation');
end

% take steepest descent as initial search direction
p = -g;  g_dot_g = dot(g,g);

% begin iterations
for iter=0:OptParam.iter_max
    
    % if necessary, add to trajectory
    if(nargout >= 5)
        x_traj(:,iter+1) = x;
    end
    
    % check for convergence
    if(sqrt(g_dot_g) <= OptParam.abs_tol)
        iflag = 1;
        return;
    end
    
    % perform weak Armijo line search
    
    % compute a_max from max. allowable change during line
    % search step
    a_max = OptParam.dx_line_max / median(abs(p));
    
    % fit quadratic polynomial to cost function along
    % line and use minimum to start weak line search
    c0 = F;
    c1 = dot(p,g);
    if(c1 > 0)
        error('search direction is not descent direction');
    end
    F2 = feval(func_name,x+a_max*p,ModelParam);
    c2 = (F2-c0-c1*a_max)/(a_max^2);
    if (c2 > 0)
        a_max = -c1/(2*c2);
    end 

    % begin Armijo weak line search iterations
    a = a_max;
    for iter_line = 0:OptParam.iter_line_max
        
        % compute cost function and gradient
        [F2,g2,iOK] = feval(func_name,x+a*p,ModelParam);
        if(~iOK)
            error('failed cost function/gradient evaluation');
        end
        
        pass_all = 1;
        % check descent criterion
        if(F2 < F)
            pass_descent = 1;
        else
            pass_descent = 0; pass_all = 0;
        end
        % check acceptable rate of descent
        if( abs((F-F2)/a) >= OptParam.sigma*(-dot(g,p)) )
            pass_rate = 1;
        else
            pass_rate = 0; pass_all = 0;
        end        
        % check if all conditions passed
        if(pass_all)
            break;  % terminate line search
        else
            a = a/2;
        end
        
    end
    
    % set new estimate
    x = x + a*p;
    g_dot_g_old = dot(g,g);
    g_dot_g2 = dot(g,g2);
    F = F2; g = g2;
    g_dot_g = dot(g,g);
    
    % generate new search direction using either steepest descent,
    % Polak-Ribiere conjugate gradient, or mixed method
    if(OptParam.method==0)  % steepest descent
        p = -g;
    else  % CG-PR
        beta = (g_dot_g - g_dot_g2)/g_dot_g_old;
        if(OptParam.method==2)
            if(abs(g_dot_g2) > 0.1*g_dot_g)
                beta = 0;
            end
        end
        p = -g + beta*p;
    end
    
end

return;
