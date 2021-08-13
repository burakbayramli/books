function [x_amg] = amg_v_cycle(f, grid_data, smoother_data, max_Clevels, x, level)
%AMG_V_CYCLE performs one AMG V-cycle
%
% x = amg_v_cycle(f, grid_data, smoother_data, max_Clevels, x0)
%
% necessary arguments
% -------------------
% grid_data     obtained using amg_setup
% smoother_data obtained using amg_smoother_setup
% f             right hand side of the system to be solved
%
% optional arguments
% ------------------
% x0            starting vector
% max_Clevels   maximum number of coarse levels before calling a direct solver

% things to do:
% change so that LD and U are calculated in amg_setup and stored in
% grid_data - at the moment they are recalculated every V cycle

% MODIFICATIONS
% -------------
% v1.1 - 14/04/05
% Reverse GS sweep modified to deal with asymmetric matricies
%
% v1.2 - 18/04/05
% New input argument to define the current level
% Modified to cope with incomplete coarsening data - if the coarsening does
% not go to 1 point a direct solve is called automatically
%
% v2.0 - 27/05/05
% Changed to use smoother_data generated using amg_smoother_setup and to
% use function amg_smoother for smoothing
%
% v2.1 - 02/08/05
% Changed to use matricies in grid_data directly rather than making local
% copies
%    PIFISS function: DJS; 31 January 2007. 
%  Copyright (c) 2007 by J. Boyle
global amg_iteration_count

warning = 0;

errtol = 10;

if nargin < 6
    level = 1;
end

if level==1
    amg_iteration_count = amg_iteration_count + 1;
end

s_A = length(grid_data(level).A);

if nargin < 5
    x = zeros(s_A, 1);
    if nargin < 4
        max_Clevels = -1;    
    end
end

% Direct solve - call a direct solver if max_Clevels==0 or there are no more coarse levels 
% ------------
if max_Clevels == 0 | level == length(grid_data)
    x_amg = grid_data(level).A \ f;
    return

% AMG solve
% ---------
else    
     % Find fine grid residue
     % ----------------------
     r = f - grid_data(level).A * x;
     
     if warning == 1
         rn = norm(r);
         if rn > 10^errtol
             lrn = log10(rn);
             fprintf('Warning: v-cycle no %i at level %i. At start log10(norm(r)) is: %10.4f \n', amg_iteration_count, level, lrn) ;
         end
     end
     
    % Pre-smoothing 
    % -------------
    e = amg_smoother(f, grid_data(level).A, smoother_data(level), 'pre');
            
    if warning == 1
        en = norm(e);
        if en > 10^errtol
            len = log10(en);
            fprintf('Warning: v-cycle no %i at level %i after pre smoothing: log10(norm(e)) is: %10.4f \n', amg_iteration_count, level, len);
        end
    end
    
    x = x + e;
    
    % Find fine grid residue
    % ----------------------
    r = f - grid_data(level).A * x;
    
    if warning == 1
        rn = norm(r);
        if rn > 10^errtol
            lrn = log10(rn);
            fprintf('Warning: v-cycle no %i at level %i after pre smoothing: log10(norm(r)) is: %10.4f \n', amg_iteration_count, level, lrn) ;
        end
    end
    
    % Restrict residue
    % ----------------
    r_c  =  (grid_data(level+1).I_CF)' * r;
    
    if warning == 1
        rn = norm(r);
        if rn > 10^errtol
            lrn = log10(rn);
            fprintf('Warning: v-cycle no %i at level %i. After restricting residual log10(norm(r_c)) is: %10.4f \n', amg_iteration_count, level, lrn);
        end
    end
    
    % Find solution on coarse grid
    % ----------------------------
    if length(r_c) == 1 % case when coarse grid has 1 point
        e_c = grid_data(level+1).A \ r_c;        
    else
        x0 = zeros(length(r_c),1);
        e_c = amg_v_cycle(r_c, grid_data, smoother_data, max_Clevels, x0, level+1);
    end
    
    if warning == 1
        en = norm(e_c);
        if en > 10^errtol
            len = log10(en);
            fprintf('Warning: v-cycle no %i at level %i. After coarse solve log10(norm(e_c)) is: %10.4f \n', amg_iteration_count, level, len);
        end
    end
    
    
    % Prolongation of error
    % ---------------------
    e = grid_data(level+1).I_CF * e_c;
    
    if warning == 1
        en = norm(e);
        if en > 10^errtol
            len = log10(en);
            fprintf('Warning: v-cycle no %i at level %i. After prolongation log10(norm(e)) is: %10.4f \n', amg_iteration_count, level, len);
        end
    end
    
    % Add correction to x
    % -------------------
    x = x + e;
    
    if warning == 1
        r = f - grid_data(level).A * x;
        rn = norm(r);
        if rn > 10^errtol
            lrn = log10(rn);
            fprintf('Warning: v-cycle no %i at level %i. After adding correction log10(norm(r)) is: %10.4f \n', amg_iteration_count, level, lrn) ;
        end
    end
    
    
    % Post smoothing 
    % --------------
    x = amg_smoother(f, grid_data(level).A, smoother_data(level), 'post', x);
    
    if warning == 1
        r = f - grid_data(level).A * x;
        rn = norm(r);
        if rn > 10^errtol
            lrn = log10(rn);
            fprintf('Warning: v-cycle no %i at level %i. After post smooth log10(norm(r)) is: %10.4f \n', amg_iteration_count, level, lrn) ;
        end
    end
    
end

x_amg=x;





