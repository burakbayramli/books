function [smoother_params] = amg_smoother_params(amg_grid, smoother_type, no_sweeps)
%AMG_SMOOTHER_PARAMS generates structure for AMG smoother parameters
%
% For use with function AMG_SMOOTHER_SETUP 
%
% smoother_params = amg_smoother_params(amg_grid, smoother_type, no_sweeps)
%
% This function takes as input: amg_grid (generated using
% 'amg_grids_setup'), a smoother type, and the number of sweeps, and
% returns a structure containing smoothing paramaters which can be used as
% input for function 'amg_smoother_setup'. The structure smoother_params
% can then be edited to modify the amg sweep options.
%
% Input parameters
% ----------------
% amg_grid: generate using 'amg_grids_setup'
% smoother_type: optional argument which may take the following values
% 'LGS/PGS' line Gauss Seidel smoothing on F level (works for IFISS Q1
%           elements only) with point Gauss Seidel on coarse levels. With ndir=2
%           each application of the LGS smoother consists of an x and a y sweep.
% 'LGS/PDJ' line Gauss Seidel smoothing on F level (works for IFISS Q1
%           elements only) with point damped Jacobi on coarse levels. With ndir=2
%           each application of the LGS smoother consists of an x and a y sweep.
% 'PGS'     point Gauss Seidel
% 'ILU'     incomplete LU
% 'PDJ'     point damped Jacobi
% 'DPGS'    directional point Gauss Seidel where each application of the
%           smooother sweep can consist of an x and a y point Gauss-Seidel
%           sweep.
% 
% no_sweeps: optional argument to set number of time the pre & post smoother is applied
%
% Default values are
% smoother_type     'PDJ'
% no_sweeps         1 for LGS, ILU & DPGS and 2 for PDJ and PGS
%
% Output parameters
% -----------------
% smoother_params:  structure array where smoother_params(n) contains the
%                   smoother parameters for level==n
%
% componants of smoother_params are:
% smoother_params(n).type       type of smoother
% smoother_params(n).nsweeps    number of times smoother is applied (post and pre smoothing)
% smoother_params(n).pre        pre-sweep direction ('forward' or 'backward')
% smoother_params(n).post       post-sweep direction ('forward' or 'backward')
% smoother_params(n).ndir       number of directions for LGS or DPGS (1-2)
%                               default value is 2
% smoother_params(n).damping    damping for PDJ, default value is 0.5
% smoother_params(n).order{m}   used with DPGS, order{m} contains the sweep ordering
%                               for direction m. This function returns an empty array for order{1} since this
%                               results in x sweeping, and stores y ordering in order{2}
%                               (Note: works with IFISS structured meshes)
%    PIFISS function: DJS; 31 January 2007. 
%  Copyright (c) 2007 by J. Boyle
if nargin < 2
    smoother_type = 'PDJ'
end

no_levels = length(amg_grid);

if nargin < 3
    if ( strcmp(smoother_type,'ILU') | strcmp(smoother_type,'DPGS') )
        no_sweeps = 1;
    else
        no_sweeps = 2;
    end 
end

smoother_params = [];
damping = 0.5;  % damping for PDJ
ndir = 2;       % number of directions for LGS

for level = 1:no_levels
    if level == 1
        if ( strcmp(smoother_type,'LGS/PGS') | strcmp(smoother_type,'LGS/PDJ'))
            smoother_params(level).type = 'LGS';
            smoother_params(level).ndir = ndir;
            if nargin < 3
                smoother_params(level).nsweeps = 1;
            else
                smoother_params(level).nsweeps = no_sweeps;
            end
        else
            smoother_params(level).nsweeps = no_sweeps;
            smoother_params(level).type = smoother_type;
        end
                
        smoother_params(level).pre = 'forward';    
        smoother_params(level).post = 'backward';
        
        if  strcmp(smoother_type,'DPGS')
            smoother_params(level).ndir = ndir;
        end
        
        if  strcmp(smoother_type,'PDJ')
            smoother_params(level).damping = damping;
        end
    else
        if strcmp(smoother_type,'LGS/PGS')
            smoother_params(level).type = 'PGS';
        elseif strcmp(smoother_type,'LGS/PDJ')
            smoother_params(level).type = 'PDJ';
            smoother_params(level).damping = damping;
        else
            smoother_params(level).type = smoother_type;
        end
        
        smoother_params(level).nsweeps = no_sweeps;
        
        smoother_params(level).pre = 'forward';    
        smoother_params(level).post = 'backward';
        
        if  strcmp(smoother_type,'DPGS')
            smoother_params(level).ndir = ndir;
        end
        
        if  strcmp(smoother_type,'PDJ')
            smoother_params(level).damping = damping;
        end
    end
end

% set up ordering for 'DPGS' - directional point Gauss Seidel
% Note: I need to modify this if I'm to use aggressive coarsening in amg
if strcmp(smoother_type, 'DPGS')
    for level = 1:no_levels
        order = {[]};
        % set up sweep orders for fine level
        if level == 1   
            % first direction
            order(1) = {[]};
            
            % second direction
            s_A = length(amg_grid(level).A);
            line = sqrt(s_A);
            ord1 = (1:line:s_A);
            for i = 0:line-1
                ord2( 1+i*line : (i+1)*line) = ord1+i;
            end
            order(2) = {ord2};
            % store in smoother_params
            smoother_params(level).order = order;
            
        else
            % find sweep orders for coarse levels    
            % first direction...
            order(1) = {[]};
            
            % 2nd direction
            n_FP = size(amg_grid(level-1).A,1);             % size of A in previous level
            n_CP = size(amg_grid(level).A,1);               % size of A in current level
            old_order = smoother_params(level-1).order{2};  % order in previous level
            map_to_C = amg_grid(level).map_to_C;            % mapping from previous level to current level
            new_order = zeros(n_CP,1);
            % run through points
            new_pos = 1;
            for old_pos = 1:n_FP
                pt_F = old_order(old_pos);
                if map_to_C(pt_F) ~= 0
                    pt_C = map_to_C(pt_F);        % find point number on current level
                    new_order(new_pos) = pt_C;    % put in new order
                    new_pos = new_pos + 1;
                end
            end
            order(2) = {new_order};
            smoother_params(level).order = order;
        end
    end
    % use this to play with orders in DPGS
    %     for level = 1:length(Amg_grid)
    %         order = {[]};
    %         if level == 1
    %             order = smoother_params(level).order(2);
    %         else
    %             order = smoother_params(level).order(2);
    %             
    %         end
    %         smoother_params(level).order = order;
    %     end
end
