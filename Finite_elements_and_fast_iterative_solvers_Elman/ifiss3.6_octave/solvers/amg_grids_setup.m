function [grid_data] = amg_grids_setup(A, i_method, max_levels);
%AMG_GRIDS_SETUP performs algebraic multigrid setup phase
%			and returns a structure grid_data for use by amg_solver
%
% For use with AMG_V_CYCLE
%
% amg_grids_setup(A, i_method, max_levels)
%
% Input argument A must be defined. The function then finds A_C and the prolongation
% matrix I_CF for all coarser grids by recursively calling itself until there is only
% one C point, or until the coarsening fails.
% A_C and I_CF for each grid are stored in structure grid_data(level) where level
% defines the grid level (in order of coarseness) and level 1 is the finest level.
%
% Optional input arguments
% ------------------------
% i_method: interpolation method
% 0=BHM direct, 1=Stuben direct, 2=Stuben standard (default is 1)
%
% max_levels: the maximum number of coarse levels generated
%
% level: sets the current grid level and should not be used when calling
% this method externally (level 1: initial problem, levels 2 onwards
% correspond to coarse grids, etc)
%   IFISS function: DJS; 6 December 2009.
% Copyright (c) 2005 J. Boyle, D.J. Silvester



% MODIFICATIONS
% -------------
% v1.1 - 6/1/05
% Faster testing for strong connections
% (new version runs through columns of A rather than rows)

% v1.2 - 14/2/05
% bug fixed for i_method == 2
% (Previously A_hat was full leading to crashes from insufficient memory)
% further optimisation also implemented 

% v1.3 - 28/2/05
% bug fixed for i_method == 1
% previously the value of no_nz in function d_interp was incorrectly
% calculated as strong C-C connections could be included in str_C

% v1.2 - 14/4/05
% Modified to deal with asymmetric matricies.
% Modified so that undefined points are set to be C at the end of the
% coarsening
% Modified so that if there are no negative connections or the coarsening
% stagnates, coarsening stops at that level

% v1.3 - 20/5/05
% Corrections so that i_method 2 works correctly with asymmetric matricies
% Aggressive coarsening can now be controlled using parameter c_steps which
% sets the number of coarsening steps per level.
% Also changed so that additional coarse levels are performed in a loop
% rather than calling recursively.

% v1.4 - 23/8/05
% Modified to include a map_to_F

% REFERENCES
% ----------
% Throughout I refer to the main sources used as.....
%
% BHM
% ---
% A Multigrid Tutorial (2nd ed), Chapter 8: Algebraic Multigrid (AMG)
% by Briggs, Henson and McCormick 
%
% Stuben
% ------
% Algebraic Multigrid (AMG): An Introduction with Applications
% An appendix in 'Multigrid' by Trottenberg et al, and  
% readily available online in pdf format.


% INTERPOLATION
% -------------
% Currently three alternative interpolation schemes are implemented...
%
% i_method 0 - Direct interpolation  
% ----------
% Described in Briggs, Henson & McCormickEqn (see eqn 8.12)
%
% i_method 1 - Stuben's direct interpolation method
% ----------
% I_CF is found using eqns (120) & (121) applied directly
%
% i_method 2 - Stuben's standard method
%----------
% I_CF found using eqns (120) & (121) applied after first replacing each
% strongly connected F point (see section 7.2.1). This increases C points
% to include those which have no direct connection.


% Additional notes
% ----------------
% In i_methods 0 & 2 positive connections are added to the diagonal before 
% calculating interpolates and so are not counted as connections. Method 1
% adds additional C points to allow positive interpolation as described by
% Stuben if have_pC == true, otherwise adds them to the diagonal. 
% 
% i_method 0 & 1 require a two pass coarsening scheme, since for every F-F
% strong connection there must be a common C point for interpolation.


% THIS FUNCTION CURRENTLY DOES NOT...
%
% 1. Jacobi relaxation for interpolation - we may want this, its supposed 
% to improve interpolation and convergence (Stuben Section 7.2.3).
%
% 2. Truncation of interpolation - reduces the amount of calculation, it may
% reduce convergence, not sure we need this at the moment.

% Reminder (terminology)
% 'Point i is strongly coupled/connected to point j' means the value at point i is strongly
% influenced by point j, i.e. A_ij is large  

%% DJS modification 
% Output is switched on by resetting iout=1 
iout=0; 


if nargin < 3
    max_levels = inf;
    if nargin < 2
        i_method = 1; % default interpolation method
        if nargin < 1
            error('Insufficient input arguments')
        end
    end
end


% if amg_setup has been called with max_levels==0 simply put A into grid_data and return 
if max_levels == 0
    grid_data(1).A = A;
    return
end

have_pC = false; % true==generate C points from strong positive connections in method 1
c_steps = 1; % number of coarsening step to perform per level

level = 2;   % since first C level is level 2
n_C = 0;     % number of coarse points   

fprintf('AMG grid coarsening ... ')
while (level <= max_levels+1) & (n_C ~= 1)  
    
    % store A in grid_data
    if level == 2
        grid_data(1).A = A;
    end
            
    for c_s = 1:c_steps
       if iout, fprintf('\n\nLevel %i step %i coarsening started\n', level, c_s), end
        % SET UP INITIAL PARAMETERS
        % -------------------------
        fails = false;
        n_th = 0.25;        % defines threshold for strong negative connections
        p_th = 0.5;         % defines threshold for strong positive connections
        
        nnz_A = nnz(A);  	% number of non zero entries in A
        s_A = length(A);    % dimension of matrix A
        
        g_weight = zeros(s_A,1);    % stores grid weights (used to generate coarse matrix)
        
        str = zeros(1,s_A);         % col i stores points that strongly influence point i (-ve connections)
        str_F = zeros(1,s_A);		% col i stores F points that strongly influence point i (-ve connections)
        str_C = zeros(1,s_A);		% col i stores C points that strongly influence point i (-ve connections)
        str_t = zeros(1,s_A);       % col i stores points that i strongly influences (-ve connections)
                
        n_str = zeros(s_A,1);	% index i stores number of points that strongly influence point i (-ve connections)
        n_str_F = zeros(s_A,1);	% index i stores number of F points that strongly influence point i (-ve connections)
        n_str_C = zeros(s_A,1);	% index i stores number of C points that strongly influence point i (-ve connections)
        n_str_t = zeros(s_A,1); % index i stores number of points strongly influenced by point i (-ve connections)
        n_weak = zeros(s_A,1);  % index i stores number of points that weakly influence point i (-ve connections)
        n_weak_p = zeros(s_A,1);% index i stores number of points that weakly influence point i (+ve connections)  
        n_str_p = zeros(s_A,1); % index i stores number of points that strongly influence point i (+ve connections)                 
        
        % FIND & RECORD CONNECTION TYPE BETWEEN POINTS
        % --------------------------------------------
        % ie weak or strong connections
        % if strong positive connections exist we also record these
        
        p_flag = false;  % true if any positive connections are found
        ps_flag = false; % true if strong positive connections exist
        
        % Test for strong / negative connections
        is_negative = (A < 0);
        
        % transpose A since it is quicker to access the matrix A via columns
        A = A';
        
        % Find threshold for strong negative connections (as defined in Stuben)
        threshold = n_th * min(A);
        
        is_negstrong = sparse([],[],[],s_A,s_A,nnz_A); % preallocate
        for i = 1:s_A
            is_negstrong(:,i) =  A(:,i) < threshold(i);
        end
        is_negstrong = is_negstrong';
        
        % untranspose A
        A = A';
        
        % Record if connections are strong or weak 
        sum_pos = zeros(s_A,1);     % row sum of positive elements (to be added to diagonal)
        sum_weak = zeros(s_A,1);    % row sum of weak values
        [r,c]=find(A);      % Find indicies of non-zero entries in A
        for index=1:nnz_A % run through the connections
            i = r(index); % for point i
            j = c(index); % find the influence (strong or weak) of point j
            % ignore diagonal entries
            if j ~= i
                % Test if the connection is strong
                if is_negstrong(i,j)
                    % Strong negative connection (j strongly influences i)
                    n_str_t(j) = n_str_t(j) + 1;
                    str_t( n_str_t(j) ,j ) = i;
                    n_str(i) = n_str(i) + 1;
                    str( n_str(i) ,i ) = j;
                elseif is_negative(i,j)  % Weak negative connection
                    n_weak(i) = n_weak(i) + 1;
                    %weak( n_weak(i),i ) = j;
                    sum_weak(i) = sum_weak(i) + A(i,j);
                else            % Positive connections exist - test if strong  
                    if p_flag == false                  % Set up stuff needed for +ve connections
                        p_flag = true;
                        Amdt = (A - diag(diag(A)))';    % A minus diagonal transposed
                        threshold_p = p_th * max(Amdt); % find strong +ve thresholds
                        for index2 = 1:s_A                  % test for strong +ve connections
                            is_posstrong(:,index2) =  Amdt(:,index2) > threshold_p(index2);
                        end
                        is_posstrong = is_posstrong';
                    end
                    if is_posstrong(i,j)        % Case of +ve strong connections
                        if ps_flag == false     % Set up stuff needed for strong +ve connections
                            ps_flag = true;
                            % set up storage for positive F and C points
                            str_p = zeros(s_A,1);       % strong +ve connections to a point
                            n_str_t_p = zeros(s_A,1);   % number of strong +ve connections
                            str_t_p = zeros(s_A,1);     % strong +ve connections from a point
                            n_str_C_p = zeros(s_A,1);   % number of strong +ve C points connected
                            str_C_p = zeros(s_A,1);		% strong +ve C points
                        end
                        % store str_p connections                   
                        n_str_p(i) = n_str_p(i) + 1;
                        str_p( i,n_str_p(i) ) = j;
                        % store str_t_p connections
                        n_str_t_p(j) = n_str_t_p(j) + 1;
                        str_t_p( j,n_str_t_p(j) ) = i;
                    else
                        % weak positive connections
                        n_weak_p(i) = n_weak_p(i) + 1;
                    end            
                end;
            end;
        end
        clear is_negative is_negstrong r c threshold is_posstrong threshold_p Amdt
        
        % GENERATE COARSE GRID
        % --------------------
        % Based on negative connections only
        
        % Note that coarse (C) points are in both the fine and coarse grids
        % the fine (F) points are in the fine grid only.
        
        % pt_type(i) records whether point i is Coarse (2), Fine (1), undecided (0) or
        % unconnected (-1)	
        pt_type = zeros(s_A,1);
        
        % REMINDER
        % str(:,i) stores the points that strongly influence i
        % str_t(:,i) stores the points that i influences strongly
        
        % Identify points with no connections (these need no interpolation)
        pt_type = -full(n_str==0 & n_weak==0 & n_weak_p==0 & n_str_p==0);
        
        % First pass 
        % ----------
        g_weight = sparse(n_str_t);     % set starting values of grid weights
        w_inc = 1;				% amount to increase g_weight for each strong connection to an F point
        % w_inc=1 in BHM & 2 in Stuben
           
        [max_wt, max_pos] = max(g_weight);      % find points with maximum value of g_weight
        
%         for tj = 0:full(max_wt)
%             fprintf('\n\nWeight %i\n', tj)
%             disp('----------')
%             for ti = 1:s_A
%                 if (g_weight(ti) == tj)
%                     fprintf('\nPoint %i',ti)
%                 end
%             end
%         end
        
        if max_wt == 0  % This should not arise for M-matricies
            if c_s == 1     % no I_CF can be formed for this level
                last_level = level-1;
                fails = true;
            else            % I_CF already exists for this level from previous step
                last_level = level;
            end
            % warning message
            if iout, 
            fprintf('\nWarning: no negative connections exist for coarsening step %i on level %i', c_s, level)
            fprintf('\nCoarsening has stopped: last viable coarse level is level %i\n', last_level)
            end
            % exit coarsening
            max_levels = 0; % to stop coarsening process
            break
        end
                
        while max_wt > 0                        
            new_C = max_pos(1);		% choose a new C point
            g_weight(new_C) = 0;   	% set weight for new C point to zero
            pt_type(new_C) = 2;
            % remove any strong C-C connections as these are ignored by interpolation
            if n_str_C(new_C) ~= 0
                n_str_C(new_C) = 0;
                str_C(:,new_C) = 0;
            end
            
            % Run through all points (pt_i) which new_C strongly influences 
            for index_i = 1:n_str_t(new_C)
                pt_i = str_t( index_i,new_C );
                if pt_type(pt_i) ~= 2   % bypass strong C-C connections
                    % Record the new C point in str_C(pt_i,:)
                    n_str_C(pt_i) = n_str_C(pt_i) + 1;
                    str_C( n_str_C(pt_i),pt_i ) = new_C;       
                    
                    % find new F points
                    if pt_type(pt_i) == 0    % if pt_i is undecided
                        pt_type(pt_i) = 1;   % set pt_i to be F
                        g_weight(pt_i) = 0;  % set g_weight for pt_i to be zero
                        % Record pt_i in str_F(pt_j,:) for all points (pt_j) that pt_i strongly influences
                        for index_j = 1:n_str_t(pt_i)
                            pt_j = str_t( index_j,pt_i );
                            n_str_F(pt_j) = n_str_F(pt_j) + 1;
                            str_F( n_str_F(pt_j),pt_j ) = pt_i;
                        end
                        % increment weights of all undecided points (pt_k) which strongly influence pt_i
                        for index_k = 1: n_str(pt_i)
                            pt_k = str(index_k ,pt_i);
                            if pt_type(pt_k) == 0
                                g_weight(pt_k) = g_weight(pt_k) + w_inc;
                            end
                        end
                    end        
                end
            end 
            [max_wt, max_pos] = max(g_weight);     % find new points with maximum weight
        end
         
        if iout, fprintf('C points after 1st pass: %i\n',sum(pt_type==2)), end
        
        % Second pass
        % -----------
        % for BHM method and direct interploation
        % ensures: for each F point pt_i all strongly connected F points share a C point
        if i_method < 2
            % Run through each point (pt_i)
            for pt_i = 1:s_A            % run through the points
                if pt_type(pt_i)==1		% Ignore C points
                    n_fails = 0; 		% Number of test fails for the pair of F points
                    
                    % Run through each F point (pt_j) with a strong connection to pt_i
                    for index_j = 1:n_str_F(pt_i)              
                        % Bypass if testing has failed twice already
                        if n_fails < 2  
                            pt_j = str_F( index_j,pt_i );
                            % perform test
                            test = false;
                            index1 = 1;
                            while (test==false) && ( index1 <= n_str_C(pt_i) )
                                index2 = 1;
                                while (test==false) && ( index2 <= n_str_C(pt_j) )
                                    test = ( str_C(index1,pt_i) == str_C(index2,pt_j) );
                                    index2 = index2 + 1;
                                end
                                index1 = index1 + 1;
                            end
                            
                            %test = any( ismember(str_C(pt_i,1:n_str_C(pt_i)), str_C(pt_j,1:n_str_C(pt_j)) ) );
                            % If test fails
                            if test == false
                                n_fails = n_fails + 1;
                                if n_fails == 1 % if test fails only once this pt_j will become C
                                    new_C = pt_j;
                                end
                            end
                        end
                    end
                                       
                    if n_fails > 0
                        if n_fails == 2
                            new_C = pt_i;
                        end
                        pt_type(new_C) = 2; 
                        
                        % remove any strong C-C connections as these are ignored
                        if n_str_C(new_C) ~= 0
                            n_str_C(new_C) = 0;
                            str_C(:,new_C) = 0;
                        end
                        % Update str_C and str_F for points strongly influenced by new_C
                        for index_k = 1:n_str_t(new_C)
                            pt_k = str_t( index_k,new_C );
                            if pt_type(pt_k) == 1 
                                % Add new_C to str_C(pt_k,:)
                                n_str_C(pt_k) = n_str_C(pt_k) + 1;
                                str_C( n_str_C(pt_k),pt_k ) = new_C;
                                % Remove new_C from str_F
                                n_str_F(pt_k) = n_str_F(pt_k) - 1;
                                str_F(:,pt_k) = 0;
                                str_F( 1:n_str_F(pt_k),pt_k ) = setdiff( str( 1:n_str(pt_k),pt_k ), str_C( 1:n_str_C(pt_k),pt_k ) );
                            end
                        end
                    end
                end
            end
        end
        
        if iout, fprintf('C points after 2nd pass: %i\n',sum(pt_type==2)), end
        
        % Stuben Remark 7.2 - some points may remain undecided. These are not
        % strongly influenced by any C point AND don't strongly influence any F
        % point AND don't strongly influence each other. If they are
        % strongly influenced by any F point(s) Stuben suggests calling them F
        % points and using indirect (i.e. standard) interpolation.
        % If this is implemented with direct interpolation I need to change
        % this. SO CHANGE IT
        if any(pt_type==0)
            pt_type( (pt_type==0) & (n_str>0) ) = 1;
        end
        
        % Third pass
        % ----------
        % Stuben's method for dealing with strong positive connections:
        % create new coarse (C_p) points and use +ve interpolation (this is only
        % implemented for use with Stubens direct interpolation at the moment).
        if ps_flag == 1 & i_method == 1 & have_pC == true
            % Stuben suggests using the largest positive connections to define the 
            % new coarse points. I've not done this, I choose the point with the
            % largest number of positive connections to a C_p (similar to the method 
            % used to generate C points in the first pass) and then any F point with
            % a strong positive connection to this point uses it for +ve interpolation
            
            p_weight = n_str_t_p;     % set starting values for grid weights
            [max_wt, max_pt] = max(p_weight);    
            
            %p_weight = A .* is_posstrong;
            %[max_wt max_pt] = max(max(p_weight,[],2));
            
            while max_wt > 0            % Repeat untill no more points left
                C_p = max_pt(1);		% choose a +ve C point     
                p_weight(C_p) = 0;   	% set weight of new C point to zero
                
                % Run through points (pt_i) which C_p strongly influences via a
                % +ve connection and record in str_C_p 
                for index = 1:n_str_t_p(C_p)
                    pt_i = str_t_p(C_p,index);
                    if pt_type(pt_i) == 1 % for fine points
                        % Record the new C point in str_C_p(pt_i,:)
                        n_str_C_p(pt_i) = n_str_C_p(pt_i) + 1;
                        str_C_p(pt_i, n_str_C_p(pt_i)) = C_p;     
                    end
                    p_weight(pt_i,:) = 0;  % set g_weight for pt_i to be zero
                end
                
                % see if C_p is a new C point and deal with this if necessary
                if pt_type(C_p) ~= 2
                    pt_type(C_p) = 2;
                    % Update str_C and str_F for points strongly influenced by the new C
                    for index = 1:n_str_t(C_p) % run through negative strong connections
                        pt_i = str_t( index,C_p );
                        if pt_type(pt_i) == 1 % for fine points
                            % Add new_C to str_C(pt_k,:)
                            n_str_C(pt_i) = n_str_C(pt_i) + 1;
                            str_C( n_str_C(pt_i),pt_i ) = C_p;
                            % Remove new_C from str_F
                            n_str_F(pt_i) = n_str_F(pt_i) - 1;
                            str_F(:,pt_i) = 0;
                            str_F( 1:n_str_F(pt_i),pt_i ) = setdiff( str( 1:n_str(pt_i),pt_i ), str_C( 1:n_str_C(pt_i),pt_i ) );
                        end
                    end
                end
                
                %[max_wt, max_pt] = max(max(p_weight,[],2));     % find new points with maximum weight
                [max_wt, max_pt] = max(p_weight);    
            end
            if iout, fprintf('C points after 3rd pass: %i\n',sum(pt_type==2)), end
        end
        
        clear g_weight n_weak n_weak_p n_str_p n_str_t n_str_t_p str_t
        
        % Check if some points are neither F or C.
        % This arises if there is no strong negative connection either to or from a point. Such a point is
        % not set as a C point in the coarsening scheme since nothing interpolates from it, and can not be 
        % an F point, since it interpolates from no other point.
        % Therefore set these to be Coarse points and move onto the next coarse
        % level.
        if any(pt_type==0)  
            if iout,
            fprintf('\nWarning: some points remain neither F nor C.')
            fprintf('\nAMG is setting these to be C points to allow coarsening to continue\n') 
            end
            pt_type(pt_type==0) = 2;
        end
        
        if iout, fprintf('C points after coarsening: %i\n',sum(pt_type==2)), end
        
        % Test for stagnation
        n_C = sum(pt_type==2);
        if n_C == s_A
            if c_s == 1     % no I_CF can be formed for this level
                last_level = level-1;
                fails = true;
            else            % I_CF already exists for this level from previous step
                last_level = level;
            end
            % warning message
            if iout,
            fprintf('\nWarning: coarsening has stagnated for step %i on level %i', c_s, level)
            fprintf('\nCoarsening has stopped: last viable coarse level is level %i\n', last_level)
            end 
            % exit coarsening
            max_levels = 0; % to stop coarsening process
            break
        end
                 
        % CALCULATE INTERPOLATION MATRICIES
        % ---------------------------------
        % Methods
        % 0. Direct interpolation described in BHM
        % 1. Simple direct interpolation described by Stuben
        % 2. indirect interpolation described by Stuben
        
        % find sum of row off-diagonal positive entries 
        if i_method < 2 
            if p_flag == 1
                sum_pos = sum( (A>0).*A, 2 ) - diag(A);
                if ps_flag == 1 & i_method == 1 & have_pC == true   % modify this for i_method 1
                    sum_pos = sum_pos .* (n_str_C_p == 0);  % since we only want to add this sum when no strong +ve connections
                end
            end
        end
        
        % Generate empty I_CF matrix
        I_CF = sparse(s_A,n_C);
        
        % Find a relationship between the position of the C points
        % in the fine and coarse grids
        pos = 1;
        new_pos = zeros(s_A,1);
        old_pos = zeros(n_C,1);
        for index=1:s_A
            if pt_type(index) == 2 
                new_pos(index) = pos;
                old_pos(pos) = index;
                pos = pos+1;
            end
        end
        % attach this to grid_data
        grid_data(level).map_to_C(:,c_s) = new_pos;
        grid_data(level).map_to_F(:,c_s) = old_pos;
        
        % Stubens standard (indirect) method
        % ----------------------------------
        % First replace strong -ve F-F with indirect connections
        % by generating matrix A_hat to replace A
        if i_method == 2
            
            % Transpose A to attempt speedup of calculation    
            A = A';
            % Copy A
            A_hat = A;
            
            % Remove strong -ve F-F connections from A_hat      
            for i = 1:s_A  
                if pt_type(i) == 1
                    for index_k = 1:n_str_F(i)
                        k = str_F( index_k,i );
                        A_hat(k, i) = 0;
                    end
                end
            end
            
            % Add indirect connections to A_hat to replace the connections just removed
            n_str_C_hat = n_str_C;
            str_C_hat = str_C;
            for i = 1:s_A
                if pt_type(i) == 1
                    alpha =  A(:,i) ./ diag(A);
                    rowsum = sparse(s_A,1);
                    for index_k = 1:n_str_F(i)
                        % include indirect C points in str_C_hat for interpolation
                        k = str_F( index_k,i );
                        row = alpha(k) * A(:,k);
                        row(k) = 0;  % remove pt k (since k is not in neighbourhood of pt k)
                        rowsum = rowsum + row;
                        
                        for index_j = 1:n_str_C(k);
                            j = str_C(index_j,k);
                            % for new C points with - connection add pt_j to the -ve pt_i interpolation set
                            index = 1;
                            test = false;
                            while (index <= n_str_C_hat(i) ) && ( test == false )
                                test = (j == str_C_hat(index,i) );
                                index = index + 1;
                            end
                            if test == false
                                n_str_C_hat(i) = n_str_C_hat(i) + 1;    
                                str_C_hat(n_str_C_hat(i),i) = j;
                            end
                        end
                    end
                    A_hat(:,i) = A_hat(:,i) - rowsum;
                end
            end
            
            
            if p_flag == 1 % Remove +ve connections by adding to diagonal
                new_diag = sum( (A_hat>0).*A_hat, 1 );
                % Remove +ve connections by adding to diagonal
                A_hat = A_hat .* (A_hat < 0);
                A_hat = A_hat + diag(sparse(new_diag));
            end
            
            A_hat = A_hat';
            A= A';
            
            % call function to perform Stuben interpolation
            I_CF = d_interp(A_hat, s_A, pt_type, n_str_C_hat, str_C_hat, new_pos); %Continue as per Stubens simple method
        end
        
        % Stubens simple method
        % ---------------------
        if i_method == 1
            % add positive elements to the diagonal
            if p_flag == 1  
                A_hat = A + diag(sparse(sum_pos));
            else
                A_hat = A;
            end
            
            % call function to perform interpolation
            if ps_flag == 0 | have_pC == false
                I_CF = d_interp(A_hat, s_A, pt_type, n_str_C, str_C, new_pos);
            else
                I_CF = d_interp_p(A_hat, s_A, pt_type, n_str_C, str_C, n_str_C_p, str_C_p, new_pos);
            end
        end
        
        
        % BHM direct interpolation
        % ------------------------
        if i_method == 0  
            % Generate empty I_CF matrix
            for pt_i=1:s_A
                if pt_type(pt_i)==2     % pt_i is a C points
                    I_CF(pt_i,new_pos(pt_i)) = 1;
                elseif pt_type(pt_i)==1    % pt_i is a fine points
                    % run through points (pt_j) in str_C(pt_i,:)
                    for index_j = 1:n_str_C(pt_i)
                        pt_j = str_C( index_j,pt_i );
                        % sum contribution over F points (pt_m) connected to pt_i
                        sum_m = 0;
                        for index_m = 1:n_str_F(pt_i)
                            pt_m=str_F( index_m,pt_i );
                            % sum contribution over C points (pt_k) connected to pt_i
                            sum_k = 0;
                            for index_k = 1:n_str_C(pt_i)
                                pt_k = str_C( index_k,pt_i );
                                sum_k = sum_k + A(pt_m,pt_k);
                            end
                            if sum_k == 0
                                warning('sum_k = 0')
                            end
                            sum_m = sum_m + A(pt_i,pt_m) * A(pt_m,pt_j) / sum_k;
                        end
                        sum_n = sum_weak(pt_i) + sum_pos(pt_i); % contribution from weak & +ve connections
                        I_CF(pt_i,new_pos(pt_j)) = -(A(pt_i,pt_j)+sum_m)/(A(pt_i,pt_i)+sum_n); 
                    end
                end
            end
        end
        
        if n_C == 0 
            % no C point exist - this shouldn't happen with M-matricies
            warning('No C points exist at this level')
%             I_CF(1,1) = 1;    % fudge things
%             for i = 2:s_A
%                 I_CF(i,1) = A(i,1)/A(i,i);        
%             end
        end
        
        clear n_str n_str_C n_str_F new_pos str str_C str_F sum_pos
        clear weak sum_weak A_diag n_str_C_p A_hat
        
        % FINISH
        % ------
        % Output info to screen
        if p_flag == 1;
            if iout, fprintf('\nWeak positive connections exist'), end
            if ps_flag == 1
                if iout, fprintf('\nStrong positive connections exist'), end
            end    
        end
        if iout,
        fprintf('\nCoarsening completed (%i Coarse, %i Fine and %i Unconnected points)\n', n_C, sum(pt_type==1), sum(pt_type==-1) )
        end

        % Generate course version of A
        A = I_CF' * A * I_CF;
        
        if c_s > 1 % i.e. a coarsening step has already occured
            I_CF = I_CF_old * I_CF;
        end
        
        if c_s < c_steps % i.e. we are about to perform another coarsening step (aggressive coarsening)
            I_CF_old = I_CF;
        end
            
        
    end % end of coarsening step
    
    % Add A and I to struct
    if fails == false
        grid_data(level).A = A;
        grid_data(level).I_CF = I_CF;
    end
    
    level = level + 1;
     
end % end of this level
fprintf(' %i grid levels constructed.\n', level)
return

%==========================================================================
%==========================================================================

function I_CF = d_interp(A, s_A, pt_type, n_str_C, str_C, new_pos, sum_pos);
% Subfunction to create prolongation matrix for -ve interpolation

if nargin < 7
    sum_pos = zeros(s_A,1); 
end

% Generate empty I_CF matrix
n_C = sum(pt_type==2);
no_nz = n_C + nnz(str_C);
I_CF = zeros(no_nz,1);
is = zeros(no_nz,1);
js = zeros(no_nz,1);

diag_A = diag(A) + sum_pos;
alpha = zeros(s_A,1);           % alpha defined in Stuben

sum_N = sum( (A.*(A<0)) ,2);    % sum of all neighbourhood connections
sum_C = zeros(s_A,1);           % sum of all coarse connections

% try this for speedup
A=A';

for i = 1:s_A
    if pt_type(i) == 1 
        if n_str_C(i) > 0
            sum_C(i) = sum( A( str_C( 1:n_str_C(i),i ),i ));
            alpha(i) = sum_N(i) / sum_C(i);  
        end
    end
end

alpha_o_d = -alpha ./ diag_A;   

pos = 1;
for i = 1:s_A   % Run through the points and find values
    if pt_type(i) == 2;
        I_CF(pos) = 1;
        is(pos) = i;
        js(pos) = new_pos(i);
        pos = pos + 1;
    elseif pt_type(i) == 1;
        for index = 1:n_str_C(i)
            j = str_C(index,i);
            I_CF(pos) = alpha_o_d(i) * A(j,i);
            is(pos) = i;
            js(pos) = new_pos(j);
            pos = pos + 1;
        end
    end        
end    

I_CF = sparse(is, js, I_CF, s_A, n_C);

%==========================================================================

function I_CF = d_interp_p(A, s_A, pt_type, n_str_C, str_C, n_str_C_p, str_C_p, new_pos);
% Subfunction to create prolongation matrix for -ve & +ve interpolation

% Generate empty I_CF matrix
n_C = sum(pt_type==2);
I_CF = sparse(s_A,n_C);
diag_A = diag(A);

sum_N = sum( (A.*(A<0)) ,2);                % sum of -ve neighbourhood connections
sum_N_p = sum( (A.*(A>0)) ,2) - diag_A;     % sum of +ve neighbourhood connections

alpha = zeros(s_A,1);           % alpha & beta defined in Stuben
beta = zeros(s_A,1);

sum_C = zeros(s_A,1);  
sum_C_p = zeros(s_A,1);

for i = 1:s_A
    if pt_type(i) == 1 
        if n_str_C(i) > 0
            sum_C(i) = sum( A(i, str_C( 1:n_str_C(i),i ) ));
            alpha(i) = sum_N(i) / sum_C(i);  
        end
        if n_str_C_p(i) > 0
            sum_C_p(i) = sum( A(i, str_C_p(i,1:n_str_C_p(i))) );
            beta(i) = sum_N_p(i) / sum_C_p(i); 
        end
    end
end

i_diagA = 1 ./ diag_A;

for i = 1:s_A   % Run through the points
    if pt_type(i) == 2;
        I_CF (i,new_pos(i)) = 1;
    elseif pt_type(i) == 1  % fine point
        for index = 1:n_str_C(i)
            j = str_C(index,i);
            I_CF(i, new_pos(j)) = - alpha(i) * A(i,j) * i_diagA(i);
        end
        for index = 1:n_str_C_p(i)
            j = str_C_p(i,index);
            I_CF(i, new_pos(j)) = - beta(i) * A(i,j) * i_diagA(i);
        end
    end
end
