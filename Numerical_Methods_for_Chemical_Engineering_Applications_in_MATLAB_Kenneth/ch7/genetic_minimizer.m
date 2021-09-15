% genetic_minimizer.m
%
% [x,F,iflag,x_pop,F_pop,F_traj] = ...
%    genetic_minimizer(func_name,x0,OptParam,ModelParam);
%
% This MATLAB routine employs a simple genetic algorithm
% to minimize a cost function for a non-constrained
% problem with continuously-varying parameters.
%
% Input parameters:
% func_name: the routine name that computes the
%           cost function
% x0 : the initial guess
% OptParam: a structure containing optimization parameters
%     .num_gen = total number of generations
%     .num_pop = the population size
%     .num_survivors = the total number of survivors
%     .prob_mutate = prob. of selecting mutation
%     .std_displace = std. displacement during mutation
%     .verbose = if non-zero, print out cost function of
%                best candidate periodically
%     .metric = positive-definite metric matric
%     .dist_sq_min = min. allowable sq. distance between
%               survivors to ensure diversity
% ModelParam: structure of model parameters that may be
%       passed to the cost function vector
% K.J. Beers. MIT ChE. 8/5/03

function [x,F,iflag,x_pop,F_pop,F_traj] = ...
    genetic_minimizer(func_name,x0,OptParam,ModelParam);

iflag = 0;

% First, set OptParam to default values if necessary.
try val = OptParam.num_gen;
catch OptParam.num_gen = 100*length(x0);
end
try
    val = OptParam.num_pop;
catch
    OptParam.num_pop = 100*length(x0);
end
try
    val = OptParam.num_survivors;
catch
    OptParam.num_survivors = ...
        ceil(0.1*OptParam.num_pop);
end
try
    val = OptParam.prob_mutate;
catch
    OptParam.prob_mutate = 0.75;
end
try
    val = OptParam.std_displace;
catch
    OptParam.std_displace = 0.1*max(abs(x0));
    if(OptParam.std_displace < 1e-6)
        OptParam.std_displace = 1e-6;
    end
end
try
    val = OptParam.verbose;
catch
    OptParam.verbose = 0;
end
try
    val = OptParam.metric_matrix;
catch
    OptParam.metric = speye(length(x0));
end
try
    val = OptParam.dist_sq_min;
catch
    OptParam.dist_sq_min = 0.1*OptParam.std_displace;
end

% make sure x0 is column vector
if(size(x0,2)==1)
    x_guess = x0;
else
    x_guess = x0';
end
num_param = length(x0);

% Set as the zeroth population the
% initial guess
x_pop = zeros(num_param,OptParam.num_pop);
F_pop = zeros(1,OptParam.num_pop);
x_pop(:,1) = x0;
F_pop(1) = feval(func_name,x_pop(:,1),ModelParam);
old_survivors = 1;

% Now, iterate over the generations
F_traj = zeros(OptParam.num_gen,1);
for igen=1:OptParam.num_gen

    if(OptParam.verbose)
        if(mod(igen,OptParam.verbose)==0)
            disp(['gen # ', int2str(igen),': ', ...
                    num2str(F_pop(1))]);
        end
    end
    F_traj(igen) = F_pop(1);

    % Fill out the current generation
    for k=old_survivors+1:OptParam.num_pop
        
        % select a move
        if(k==2)  % must choose mutation
            choose_move = 0;
        else
            rand_choose = rand;
            if(rand_choose <= OptParam.prob_mutate)
                choose_move = 0;  % mutate
            else
                choose_move = 1; % cross
            end
        end
        
        % perform mutation if necessary
        if(choose_move==0)
            rand_choose = rand;
            j = 1 + round(rand_choose*(k-2));
            x_pop(:,k) = x_pop(:,j) + ...
                OptParam.std_displace*randn(num_param,1);
            
        % perform crossing of two parents
        else
            rand_choose = rand;
            j = 1 + round(rand_choose*(k-2));
            rand_choose = rand;
            l = 1 + round(rand_choose*(k-2));
            rand_vector = rand(num_param,1);
            x_pop(:,k) = x_pop(:,j);
            find_parent = find(rand_vector < 0.5);
            x_pop(find_parent,k) = x_pop(find_parent,l);
        end
        
        % compute cost function of new member
        F_pop(k) = feval(func_name,x_pop(:,k),ModelParam);
        
    end
    
    % sort by increasing cost function
    [F_sort,i_sort] = sort(F_pop);
    
    % now, go through and identify the survivors,
    % enforcing diversity constraints
    new_survivors = 0;
    x_survivors = zeros(num_param,OptParam.num_survivors);
    for k=1:OptParam.num_pop
        
        % find value in x_pop of kth best member
        k_sorted = i_sort(k);

        % check for diversity
        is_diverse = 1;
        for j=1:new_survivors
            j_sorted = i_sort(j);           
            delta_x = x_pop(:,k_sorted) - x_pop(:,j_sorted); 
            dist_sq = dot(delta_x,OptParam.metric*delta_x);
            if(dist_sq < OptParam.dist_sq_min)
                is_diverse = 0;
                break;
            end
        end
        
        % if diverse, add to survivors
        if(is_diverse)
            new_survivors = new_survivors + 1;
            x_survivors(:,new_survivors) = x_pop(:,k_sorted);
            F_survivors(new_survivors) = F_pop(k_sorted);
        end
        
        % check to see if enough survivors have been added
        if(new_survivors >= OptParam.num_survivors)
            break;
        end
                       
    end
    
    % move survivors to the population
    x_pop = zeros(size(x_pop));
    F_pop = zeros(size(F_pop));
    x_pop(:,1:new_survivors) = x_survivors;
    F_pop(1:new_survivors) = F_survivors;
end

% return best candidate
x = x_pop(:,1);
F = F_pop(1);

iflag = 1;
return;
