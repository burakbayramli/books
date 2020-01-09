% LARGE_SCALE_FLOW_CONTROL.M
% 
% solves:
%           minimize    -sum(log(f))
%           subject to  R*f <= c
%
% using a truncated netwon primal-dual interior point method


%% ----------- problem data -----------
n = 10^5;                   % number of links
m = 2*n;                    % number of flows

% routing matrix
fprintf(1,'Creating Routing matrix with m = %d, n = %d\n',m,n);
links_per_flow = 10;        % rough number of links per flow
rand('state',0);
R = sprand(m,n,links_per_flow/m);
m_over_n = ceil(m/n);
Rd = kron(ones(m_over_n,1),speye(n));
R = R+Rd(1:m,:);            % make sure there are no empty links, flows
R = R>0;                    % convert to {0,1}
mean_length = full(mean(sum(R,1)));
mean_flows_per_link = full(mean(sum(R,2)));
fprintf(1,'Done. Avg flow length= %3.3f Avg links per flow= %3.3f\n',...
           mean_length, mean_flows_per_link);

% capacity limits
c = 0.1 + 0.9*rand(m,1); 


%% ----------- primal dual IP parameters -----------
MAX_ITERS = 100;
alpha = 0.00001;
beta = 0.5;
mu = 2;
tol = .001;           % relative duality gap (U-L)/L toleration


%% ----------- PCG parameters, methods -----------
% parameters for PCG
PCG_MAX_ITER = 200;
PCG_tol = 0.1;
PCG_WARM_START = 1;    %warm start flag, set 0 to disable
PCG_DIAG_PRECOND = 1;  %diagonal preconditioning flag, set 0 to disable

% inline functions used for PCG
Afun = @(x,R,f,s,lambda)(x./f.^2+R'*((lambda./s).*(R*x)));
Mfun = @(x,R,f,s,lambda)(x./(1./f.^2+((lambda./s)'*R.^2)'));


%% ----------- initialization -----------
% primal and dual variables
f = ones(n,1);        
lambda = ones(m,1);   
while min(c-R*f)<=0   % f should be strictly feasible
    f = 0.5*f;
end
df = zeros(n,1);

% Upper and lower bounds
U = -sum(log(f));
L = n - lambda'*c + sum(log(R'*lambda));


%% ----------- history logging -----------
% keep a record of
% [iter_number, U, L, (U-L)/L, stepsize, pcg_relres, pcg_iters]
history = [0 U L (U-L)/L 0 0 0];


%% ----------- main iteration -----------
for iter = 1:MAX_ITERS
    % Determine t
    t = mu*m/(U-L);
    
    % Compute primal dual search direction
    s = c-R*f;
    x0 = zeros(n,1);
    pcg_rhs = 1./f - (1/t)*R'*(1./s);  %caching
    
    if PCG_WARM_START
        gdf = -pcg_rhs'*df; ADD_PCG_ITER = 0;
        if(gdf<0)
            x0 = (-gdf/(df'*Afun(df,R,f,s,lambda)))*df;
            ADD_PCG_ITER = 1;
        end
    end
    
    cur_pcg_tol = min(PCG_tol,(U-L)/L); % current PCG tolerance
    
    % Calculate df using PCG
    if PCG_DIAG_PRECOND
        [df,PCGflag,PCGrr,PCGiters,PCGrv] = ...
          pcg(@(x)Afun(x,R,f,s,lambda),...
              pcg_rhs,...
              cur_pcg_tol,...
              PCG_MAX_ITER,...
              @(x)Mfun(x,R,f,s,lambda),[],x0);
    else
        [df,PCGflag,PCGrr,PCGiters,PCGrv] = ...
          pcg(@(x)Afun(x,R,f,s,lambda),...
              pcg_rhs,...
              cur_pcg_tol,...
              PCG_MAX_ITER,...
              [],[],x0);
    end
    
    PCGiters = length(PCGrv)-1+ADD_PCG_ITER;

    Rdf = R*df; %caching
    dlambda = (lambda./s).*(Rdf)-lambda+(1/t)*(1./s);
     
    % Find the maximum gamma for which s-gamma*R*df>0
    % Select gamma to be .99 this value    
    ind_f = find(Rdf>0);
    gamma = 1;
    if size(ind_f) > 0
        gamma = min(1,0.99*min(s(ind_f)./Rdf(ind_f)));
    end

    % Force f+gamma*df > 0
    if min(f+gamma*df)<=0
        % Find the maximum s for which f+gamma*df > 0
        % Select gamma to be .99 this value
        ind_f = find(df<0);
        gamma = min(gamma,0.99*min(-f(ind_f)./df(ind_f)));
    end

    % Force lambda+gamma*dlambda > 0
    if min(lambda+gamma*dlambda)<=0
        % Find the maximum s for which lambda+gamma*dlambda > 0
        % Select gamma to be .99 this value
        ind_l = find(dlambda<0);
        gamma = min(gamma,0.99*min(-lambda(ind_l)./dlambda(ind_l)));
    end

    % Line search
    Rtlambda = R'*lambda; Rtdlambda = R'*dlambda; %caching
    norm_res = norm([1./f-Rtlambda; lambda.*s+(1/t)*ones(m,1)]);
    while 1
        r_dual = 1./(f+gamma*df)-(Rtlambda+gamma*Rtdlambda);
        r_central = (lambda+gamma*dlambda).*(s-gamma*Rdf)+(1/t)*ones(m,1);
        new_norm_res = norm([r_dual; r_central]);
        if new_norm_res <= (1-alpha*s)*norm_res
            break
        end
        
        if(gamma < 1e-11)
            fprintf(1,'Ran into numerical problems...\n\n')
            return
        end
        
        gamma = gamma*beta;
    end

    % Update
    f = f+gamma*df;
    lambda = lambda+gamma*dlambda;
    
    % Upper and lower bounds
    U = -sum(log(f));
    L = n - lambda'*c + sum(log(Rtlambda+gamma*Rtdlambda));
    
    % History
    % [iter_number, U, L, (U-L)/L, stepsize, pcg_relres, pcg_iters]
    status = [iter U L (U-L)/L gamma PCGrr PCGiters];
    fprintf(1,'Iteration: %2d   Relative DG: %3.3e    Stepsize: %1.3f   PCGrr: %1.3f    PCGiters: %2d\n'...
        ,status(1),status(4),status(5),status(6),status(7));
    history = [history; status];
    
    % Break condition
    if (U-L)/L <= tol
        break
    end
end
fprintf(1,'Done! Problem solved in %3d PCG iters\n\n',sum(history(:,7)));

%% plot the results
NewtonIters = history(end, 1);
Us = history(:, 2);
Ls = history(:, 3);
RelativeDualGaps = history(:, 4);
PCGiters = history(:, 7);
CumulativePCGiters = tril(ones(NewtonIters+1))*PCGiters;

% Plot U and L versus pcgiters
figure;
set(gca,'FontSize',16);
plot(CumulativePCGiters, Us, '-*b', CumulativePCGiters, Ls, '-og','LineWidth',1.5);
xlabel('PCGiter'); legend('upperbound','lowerbound');
%print -depsc flow_control_prim_dual.eps

% Plot relative duality gap vs pcgiters
figure;
set(gca,'FontSize',16);
semilogy(CumulativePCGiters, RelativeDualGaps, '-*','LineWidth',1.5);
xlabel('PCGiter'); ylabel('RelativeDualityGap');
%print -depsc flow_control_gap.eps
