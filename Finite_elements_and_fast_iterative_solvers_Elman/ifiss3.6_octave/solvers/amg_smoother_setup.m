function [smoother_data] = amg_smoother_setup(grid_data, smoother_params);
%AMG_SMOOTHER_SETUP generates smoother data for AMG
%
% smoother_data = amg_smoother_setup(grid_data, smoother_params)
%
% For use with AMG_V_CYCLE
%
% Takes grid_data generated using amg_setup and creates smoother_data
% according to structure smoother_params.
% 
% The structure for smoother_params can be generated using function AMG_SMOOTHER_PARAMS
%    PIFISS function: DJS; 5 January 2010  
%  Copyright (c) 2007 by J. Boyle

size = length(grid_data);

for level = 1:size
    % get the smoother information for this level 
    smoother = smoother_params(level);
    
    % check that line Gauss Seidel isn't called on coarse levels
    if strcmp(smoother.type,'LGS') & level ~= 1
        fprintf('\nWarning: line smoother has been applied to level %3i', level);
        fprintf('\nThis implementation of line Gauss Seidel only valid for Q1 elements and level 1')
        fprintf('\nSmoother changed to point damped Jacobi (damping=0.5) on this level\n')
        smoother.type = 'PDJ';
        smoother.damping = 0.5;
    end
    
    % attach smoother info to smoother_data
    smoother_data(level).smoother_params = smoother;
    
    % line Jacobi for Q1 elements on level 1
    if strcmp(smoother.type,'LJ')  
        n = sqrt(length(grid_data(level).A));
        % first direction
        Q1 = diag(diag(grid_data(level).A,0)) + diag(diag(grid_data(level).A,-1),-1) + diag(diag(grid_data(level).A,1),1);
        [smoother_data(level).L1, smoother_data(level).U1] = lu(Q1);
        
        % second direction
        if smoother.ndir >= 2
            Q2 = diag(diag(grid_data(level).A,0)) + diag(diag(grid_data(level).A,-n),-n) + diag(diag(grid_data(level).A,n),n);
            [smoother_data(level).L2, smoother_data(level).U2] = lu(Q2);
        end
        
    % line Gauss Seidel for Q1 elements on level 1
    elseif strcmp(smoother.type,'LGS')  
        n = sqrt(length(grid_data(level).A));
        % first direction
        Q1 = tril(grid_data(level).A,1);
        [smoother_data(level).L1, smoother_data(level).U1] = lu(Q1);
        
        % second direction
        if smoother.ndir >= 2
            Q2 = diag(diag(grid_data(level).A,0)) + diag(diag(grid_data(level).A,-n),-n) + diag(diag(grid_data(level).A,n),n) ...
                + diag(diag(grid_data(level).A,-n-1),-n-1) + diag( diag(grid_data(level).A,-1),-1) + diag(diag(grid_data(level).A,n-1),n-1);
            [smoother_data(level).L2, smoother_data(level).U2] = lu(Q2);
            
            % third direction
            if smoother.ndir >= 3
                Q3 = triu(grid_data(level).A,-1);
                [smoother_data(level).L3, smoother_data(level).U3] = lu(Q3);
                
                % fourth direction
                if smoother.ndir == 4
                    Q4 = diag(diag(grid_data(level).A,0)) + diag(diag(grid_data(level).A,-n),-n) + diag(diag(grid_data(level).A,n),n) ...
                        + diag(diag(grid_data(level).A,n+1),n+1) + diag( diag(grid_data(level).A,1),1) + diag(diag(grid_data(level).A,-n+1),-n+1);
                    [smoother_data(level).L4, smoother_data(level).U4] = lu(Q4);
                end
            end
        end
        
    % point Gauss Seidel   
    elseif strcmp(smoother.type,'PGS')
        
        % set up for forward sweep
        if strcmp(smoother.pre, 'forward') | strcmp(smoother.post, 'forward')
            smoother_data(level).LD = tril(grid_data(level).A);      % part containing D & L
        end
        
        % set up backward sweep
        if strcmp(smoother.pre, 'backward') | strcmp(smoother.post, 'backward')     
            smoother_data(level).UD = triu(grid_data(level).A);      % part containing D & U
        end
        
    % directional point Gauss Seidel
    elseif strcmp(smoother.type,'DPGS')
        % set up directional sweeps
        ord = smoother.order;
        n_dir = smoother.ndir;
        
        for dir = 1:n_dir
            if length(ord{dir}) == 0
                n = length(grid_data(level).A);
                order = (1:n);
                smoother_data(level).smoother_params.order(dir) = {order};
            else
                order = ord{dir};
            end
            
            if strcmp(smoother.pre, 'forward') | strcmp(smoother.post, 'forward')
                % first direction
                smoother_data(level).LD(dir) = {tril( grid_data(level).A(order,order) )};      % first dir - part containing D & L
            end
            
            % set up backward sweep
            if strcmp(smoother.pre, 'backward') | strcmp(smoother.post, 'backward')    
                % first direction
                smoother_data(level).UD(dir) = {triu( grid_data(level).A(order,order) )};      % part containing D & U
            end
        end
        
    % point damped Jacobi sweep 
    elseif strcmp(smoother.type,'PDJ')
        omega = smoother.damping;   % relaxation factor for damped Jacobi
        n = length(grid_data(level).A);
        smoother_data(level).D = (1/omega)*spdiags(diag(grid_data(level).A), 0, n, n);
        
        % ILU(0) sweep
    elseif strcmp(smoother.type,'ILU')
        setup.type='nofill';
        [smoother_data(level).L, smoother_data(level).U] = ilu(grid_data(level).A,setup);
    end
end
