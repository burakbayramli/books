function [x] = amg_smoother(f, A, smoother_data, type, x);
%AMG_SMOOTHER performs smoothing 
%
% This function is called by AMG_V_CYCLE
%    PIFISS function: DJS; 31 January 2007. 
%  Copyright (c) 2007 by J. Boyle
s_A = length(A);
if nargin < 5
    x = zeros(s_A,1);
end

% get smoother params for current level
smoother = smoother_data.smoother_params;

% find direction
if strcmp(type, 'pre')
    direction = smoother.pre;
elseif strcmp(type, 'post')
    direction = smoother.post;
end

% find number of sweeps
ns = smoother.nsweeps;

for s = 1:ns
    if strcmp(smoother.type,'ILU')   % ILU sweeps
        r = f - A * x;
        x = x + smoother_data.U\(smoother_data.L\r);
            
    elseif strcmp(smoother.type,'LGS')   % line Gauss Seidel
        nd = smoother.ndir; % find number of directions
        
        if strcmp(direction, 'backward')
            if nd > 3 
                % do 4th direction
                r = f - A * x;
                x = x + smoother_data.U4\(smoother_data.L4\r);
            end
            
            if nd > 2
                % do 3rd direction
                r = f - A * x;
                x = x + smoother_data.U3\(smoother_data.L3\r);
            end
            
            if nd > 1
                % do 2nd direction
                r = f - A * x;
                x = x + smoother_data.U2\(smoother_data.L2\r);
            end
        end
        
        % do first direction
        r = f - A * x;  
        x = x + smoother_data.U1\(smoother_data.L1\r);
        
        if strcmp(direction, 'forward')
            if nd > 1
                % do 2nd direction
                r = f - A * x;
                x = x + smoother_data.U2\(smoother_data.L2\r);
                if nd > 2
                    % do 3rd direction
                    r = f - A * x;
                    x = x + smoother_data.U3\(smoother_data.L3\r);
                    if nd > 3
                        % do 4th direction
                        r = f - A * x;
                        x = x + smoother_data.U4\(smoother_data.L4\r);
                    end
                end
            end
        end
        
        
    elseif strcmp(smoother.type,'PGS') % point Gauss Seidel sweeps
        if strcmp(direction, 'forward')
            r = f - A * x;
            x = x + smoother_data.LD\r;           
        elseif strcmp(direction, 'backward')
            r = f - A * x;
            x = x + smoother_data.UD\r;           
        end
        
    elseif strcmp(smoother.type,'DPGS')   % directional point Gauss Seidel
        ord = smoother.order;
        n_dir = smoother.ndir;
        
        if strcmp(direction, 'forward')
            for dir = 1:n_dir
                if length(ord{dir}) == 0
                    n = length(A);
                    order = (1:n);
                else
                    order = ord{dir};
                end
                
                r = f - A * x;
                r = r(order);
                x = x(order) + smoother_data.LD{dir}\r; 
                x(order) = x;
            end
        end
        
        if strcmp(direction, 'backward')
            for dir = n_dir:-1:1 
                if length(ord{dir}) == 0
                    n = length(A);
                    order = (1:n);
                else
                    order = ord{dir};
                end
                
                r = f - A * x;
                r = r(order);
                x = x(order) + smoother_data.UD{dir}\r;   
                x(order) = x;
            end
        end
        
    elseif strcmp(smoother.type,'LJ')   % line Jacobi
        nd = smoother.ndir; % find number of directions
        
        if strcmp(direction, 'backward')
            if nd > 1
                % do 2nd direction
                r = f - A * x;
                x = x + smoother_data.U2\(smoother_data.L2\r);
            end
        end
        
        % do first direction
        r = f - A * x;  
        x = x + smoother_data.U1\(smoother_data.L1\r);
        
        if strcmp(direction, 'forward')
            if nd > 1
                % do 2nd direction
                r = f - A * x;
                x = x + smoother_data.U2\(smoother_data.L2\r);
            end
        end
        
    elseif strcmp(smoother.type,'PDJ') % point damped Jacobi
        r = f - A * x;
        x = x + smoother_data.D\r;    
    else 
        disp('warning: no smoother specified')
    end
end

