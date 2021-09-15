% simulated_annealing.m
%
% [x,F,iflag,x_traj] = ...
%     simulated_annealing(func_name,x0,OptParam,ModelParam);
%
% This MATLAB routine implements simulated annealing to
% attempt to find the global minimum of an unconstrained
% minimization problem.
%
% The input parameters are :
% func_name : the name of the routine that computes the cost function
% x0 : the initial guess
% OptParam : a structure with parameters for the optimization routine
%   .iter_max : the max # of annealing iterations
%   .max_displace : the max. allowable step size
%   .verbose : print results every # of steps
%   .temp_init : the initial temperature
%   .do_end_min : if non-zero, finish with simplex method
%   .num_runs : if more than one (default), do multiple
%          simulated annealing runs, starting each from
%          the best minimum found to date
% K. Beers. MIT ChE. 8/2/03

function [x,F,iflag,x_traj] = ...
    simulated_annealing(func_name,x0,OptParam,ModelParam);

iflag = 0;

F = feval(func_name,x0,ModelParam);

% set OptParam to default if necessary
try
    val = OptParam.iter_max;
catch
    OptParam.iter_max = 500*length(x0);
end
try
    val = OptParam.max_displace;
catch
    OptParam.max_displace = 0.1*max(abs(x0));
    if(OptParam.max_displace < 1e-6)
        OptParam.max_displace = 1e-6;
    end
end
try
    val = OptParam.verbose;
catch
    OptParam.verbose = 0;
end
try
    val = OptParam.temp_init;
catch
    OptParam.temp_init = 0.1*F;
end
try
    val = OptParam.do_end_min;
catch
    OptParam.do_end_min = 1;
end
try
    val = OptParam.num_runs;
catch
    OptParam.num_runs = 1;
end

if(OptParam.verbose)
    disp('Begninning simulated_annealing steps ...');
    x_traj(:,1) = x0; num_frames=1;
end

% Begin Monte Carlo iterations
x = x0;
x_best = x0; F_best = F;
for irun=1:OptParam.num_runs
    % set initial value at best minimum found to date
    x = x_best; F = F_best;
    if(OptParam.verbose)
        disp(' ');
        disp(['Starting run # : ', int2str(irun)]);
    end
    
    for iter=1:OptParam.iter_max
        % set current temperature
        temp = OptParam.temp_init* ...
            (OptParam.iter_max-iter+1) / ...
            (OptParam.iter_max);
    
        % propose move
        xnew = x + OptParam.max_displace* ...
            (rand(size(x))-0.5*ones(size(x)));
    
        % calculate new cost function value
        Fnew = feval(func_name,xnew,ModelParam);
    
        % check for acceptance
        prob_ratio = exp((F-Fnew)/temp);
        prob_ratio = min(1,prob_ratio);
        if(rand <= prob_ratio)
            x = xnew;
            F = Fnew;
        end
        if(OptParam.verbose)
            if(mod(iter,OptParam.verbose)==0)
                disp(['   ',int2str(iter),':  ', ...
                        num2str(F)]);
                x_traj(:,num_frames+1) = x;
                num_frames = num_frames+1;
            end
        end
    end

    % From final result, perform local minimization
    if(OptParam.do_end_min)
        [x,F] = fminsearch(func_name,x,[],ModelParam);
    end
    
    % check to see if this local minimum is better
    % than the previously identified best
    if(F < F_best)
        x_best = x;  F_best = F;
    end
end

% return best found value
x = x_best;  F = F_best;

iflag = 1;
return;

    