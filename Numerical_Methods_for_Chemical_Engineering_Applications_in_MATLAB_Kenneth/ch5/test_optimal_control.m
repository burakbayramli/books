% test_optimal_control.m
function [TRAJ,iflag] = test_optimal_control(TRAJ0);
iflag = 0;

% set fixed parameters
PARAM.lambda = -1;  PARAM.kappa = 1;
PARAM.xSS = 1; PARAM.xSet = 2;
PARAM.uSet = 1;
PARAM.CU = 0.1;  PARAM.CH = 10;

% set FUN structure
FUN.f = 'test_f';  FUN.sigma = 'test_sigma';
FUN.pi = 'test_pi';  FUN.constraint = 'test_constraint';
FUN.u0 = 'test_u0';

% set SIM structure
SIM.tH = 10;  SIM.NS = 50;  SIM.x0 = 1;
SIM.t0 = 0;  SIM.constraint = 1;  SIM.verbose = 1;

% call optimal control routine
if(nargin >= 1)
    SIM.isRestart = 1;
    TRAJ = optimal_control(FUN,PARAM,SIM,TRAJ0);
else
    SIM.isRestart = 0;
    TRAJ = optimal_control(FUN,PARAM,SIM);
end

% plot optimal trajectory
figure; subplot(2,1,1);
plot(TRAJ.t_xtraj,TRAJ.x_xtraj);
xlabel('t');  ylabel('x(t)');
title('State trajectory');
subplot(2,1,2);
plot(TRAJ.t_xtraj,TRAJ.u_xtraj);
xlabel('t');  ylabel('u(t)');
title('Control input trajectory');

iflag = 1;
return;

% --------------------
function f = test_f(t,x,u,PARAM);

f = PARAM.lambda*(x-PARAM.xSS) + PARAM.kappa*u;

return;


% --------------------
function sigma = test_sigma(t,x,u,PARAM);

sigma = (x-PARAM.xSet)^2 + PARAM.CU*(u-PARAM.uSet)^2;

return;


% --------------------
function pi = test_pi(xH,PARAM);

pi = PARAM.CH*(xH-PARAM.xSet)^2;

return;


% --------------------
function UCON = test_constraint();

UCON.A = [];  UCON.b = [];
UCON.A_eq = [];  UCON.b_eq = [];
UCON.LB = -10;  UCON.UB = 10;
UCON.nonlcon = [];

return;


% --------------------
function u0 = test_u0(t,PARAM);

u0 = PARAM.uSet;

return;


% ==============================================
% optimal_control.m
%
% function [TRAJ,iflag] = optimal_control(FUN,PARAM,SIM,TRAJ0);
%
% This routine computes an optimal trajectory u(t)
% of M control inputs to a system governed by the
% set of ODEs dx/dt = f(t,x,u). The input trajectory
% is parameterized as a piecewise-continuous function.
% This trajectory is used to minimize the cost functional,
% F = \int_{t_0}^{t_H} { \sigma(s,u,x,) ds } + \pi(x(t_H))
%
% Input parameters
% ----------------
% FUN data structure
% FUN.f: name of routine that returns time derivative vector,
%        written in format used by MATLAB ode solvers
%             f = FUN.f(t,x,u,PARAM)
%        where PARAM is data structure of fixed system parameters
%        also specified in the input arguments
% FUN.sigma: name of routine that returns \sigma
%        sigma = FUN.sigma(t,x,u,PARAM);
% FUN.pi: name of routine that returns \pi function
%               pi = FUN.pi(xH,PARAM);  % xH = x(Sim.tH0;
% FUN.constraint:  the name of a routine that returns the constraint
%             data for the control inputs that apply to the set
%             of u at each time-step,
%                            UCON = FUN.constraint();
%             The routine then uses this data to generate the constraints
%             where UCON contains the fields UCON.A, UCON,b,
%             UCON.A_eq, UCON.b_eq, UCON.LB, UCON.UB,
%             that take the meanings given to them in fmincon.
%             constraint structures that enforce these constraints
%             on each u at every time step.
% FUN.u0: name of routine that returns initial guess of u(t) profile
%         u0 = FUN.u0(t,PARAM);
%
% PARAM data structure contains fixed system parameters, and is
%       passed to FUN.f
% 
% SIM is data structure specifing type of optimization to be performed
% SIM.tH: the horizon time
% SIM.NS: the number of subintervals
% SIM.x0: initial state
% SIM.t0: initial time
% SIM.isRestart : equals 0 if specified guess from FUN.u0 is to be made.
%             Equals 1 if TRAJ structure is to be used to generate initial
%             guess.  (default 0)
% SIM.constraint : if 0, no constraints, else if 1; there are
%             constraints, whose data is returned by the function
%             whose name is stored in SIM.confun (default 0)
% SIM.verbose: if 0, no printing to the screen.  Else, show progress of
%             calculations (default 0)
%
% TRAJ0: TRAJ structure from previous simulation, used in
%        a restart
%
% Output parameters
% -----------------
% TRAJ is a data structure containing the optimal input parameters
% TRAJ.t: SIM.NS x 1 vector of time values that partition the subintervals
% TRAJ.u: SIM.NS x DIM.M array where each row contains conrol inputs in
%          each interval
% TRAJ.t_xtraj: column vector of time for state trajectory at optimal design
% TRAJ.x_xtraj: each row contains state vector at corresponding time for
%                the dynamic trajectory resulting from the optimal design
% TRAJ.u_xtraj: each row contains the state vector at the times in the
%               optimal design
%
% Kenneth J. Beers
% MIT Department of Chemical Engineering
% May 31, 2005

function [TRAJ,iflag] = optimal_control(FUN,PARAM,SIM,TRAJ0);
iflag = 0;

% set constraint to default is necessary
if(~isfield(SIM,'constraint'))
    SIM.constraint = 0;
end
% set verbose level to default if necessary
if(~isfield(SIM,'verbose'))
    SIM.verbose = 0;
end
% set isRestart to default if necessary
if(~isfield(SIM,'isRestart'))
    SIM.isRestart = 0;
end

% determine dimension of state vector
DIM.N = length(SIM.x0);
% check that horizon time is greater than initial time
if(SIM.tH <= SIM.t0)
    iflag = -1;
    error('optimal_design: SIM.tH <= SIM.t0');
end
% determine dimension of control inputs
if(SIM.isRestart == 0)  % specified input
    u0 = feval(FUN.u0,0,PARAM);
    DIM.M = length(u0);
else  % use TRAJ from previous calculation
    DIM.M = size(TRAJ0.u,2);
end

% allocate memory for output structure
TRAJ.t = zeros(SIM.NS,1);  TRAJ.u = zeros(SIM.NS,DIM.M);
TRAJ.t_xtraj = 0;  TRAJ.x_xtraj = 0;  TRAJ.u_xtraj = 0;

% determine times that partition subintervals
if(SIM.isRestart == 0)
    SIM.dt = (SIM.tH - SIM.t0)/SIM.NS;
    for k=1:SIM.NS
        TRAJ.t(k) = SIM.t0 + k*SIM.dt;
    end
else  % if a restart
    TRAJ.t = TRAJ0.t;
    SIM.dt = TRAJ.t(2)-TRAJ.t(1);
end

% if a specified initial guess, initialize vector U
% that contains all control inputs
if(SIM.isRestart == 0)
    U0 = zeros(DIM.M*SIM.NS,1);
    pos = 0;
    for k=1:SIM.NS
        u0 = feval(FUN.u0,TRAJ.t(k),PARAM);
        U0(pos+1:pos+DIM.M) = u0;
        pos = pos + DIM.M;
    end
else  % initialize from TRAJ0 if restart
    SIM.NS = size(TRAJ0.u,1);  % overwrite SIM.NS
    U0 = zeros(DIM.M*SIM.NS,1);
    pos = 0;
    for k=1:SIM.NS
        U0(pos+1:pos+DIM.M) = TRAJ0.u(k,:)';
        pos = pos + DIM.M;
    end
end

% compute the initial cost functional value
F0 = OC_cost_fun(U0,FUN,PARAM,SIM,DIM,TRAJ);

% if no constrainsts, use fminunc; else use fmincon
% to find the optimal control input trajectory
if(SIM.constraint == 0) % no constraints
    if(SIM.verbose == 0)
        OPTIMOPT = optimset('Display','off');
    else
        OPTIMOPT = optimset('Display','iter');
    end
    % call fminunc
    [U,F,exitflag] = ...
        fminunc(@OC_cost_fun, U0, OPTIMOPT, ...
            FUN,PARAM,SIM,DIM,TRAJ);
        
else % there are control input constraints
    % get the constraint structures
    [A,b,A_eq,b_eq,LB,UB,nonlcon] = OC_constraints( ...
        SIM, DIM, FUN);
    if(SIM.verbose == 0)
        OPTIMOPT = optimset('Display','off');
    else
        OPTIMOPT = optimset('Display','iter');
    end
    % call fmincon
    [U,F,exitflag] = fmincon(@OC_cost_fun, U0, ...
            A, b, A_eq, b_eq, LB, UB, nonlcon, ... 
            OPTIMOPT, FUN, PARAM, SIM, DIM, TRAJ);    
end

% extract results of simulation and compute
% optimal control input and state trajectories
[F,TRAJ] = OC_cost_fun(U,FUN,PARAM,SIM,DIM,TRAJ);

% display result
if(SIM.verbose)
   disp(' ');
   exitflag, F,
end

iflag = exitflag;
return;


% --------------------
% This routine computes the cost functional for an
% optimal control problem.
function [F,TRAJ] = OC_cost_fun(U,FUN,PARAM,SIM,DIM,TRAJ);

% compute the state response for this design
ODEOPT = [];
[TRAJ.t_xtraj,TRAJ.x_xtraj] = ode15s(@OC_f, ...
    [SIM.t0 SIM.tH], SIM.x0, ODEOPT, ...
    U,FUN,PARAM,SIM,DIM,TRAJ);
len_xtraj = length(TRAJ.t_xtraj);

% from response, compute cost function value

% compute final state contribution
xH = TRAJ.x_xtraj(len_xtraj,:)';
pi = feval(FUN.pi,xH,PARAM);

% allocate memory for control inputs
TRAJ.u_xtraj = zeros(len_xtraj,DIM.M);

% compute cost function integrand at each
% time vlaue in TRAJ.t_xtraj
sigma = zeros(len_xtraj,1);
for k=1:len_xtraj
    t = TRAJ.t_xtraj(k);  % get current time
    % get current control inputs
    TRAJ.u_xtraj(k,:) = OC_get_u(t,U,SIM,DIM,TRAJ)';
    % compute \sigma(t,u,x,PARAM) from user-supplied
    % function
    sigma(k) = feval(FUN.sigma,TRAJ.t_xtraj(k), ...
        TRAJ.x_xtraj(k,:),TRAJ.u_xtraj(k,:),PARAM);
end
% integrate sigma over state
sigma_int = trapz(TRAJ.t_xtraj,sigma);

% compute cost functional value
F = sigma_int + pi;

% pack U into TRAJ if there are two or more output arguments
if(nargout >= 2)
    pos = 0;
    for k=1:SIM.NS
        TRAJ.u(k,:) = U(pos+1:pos+DIM.M)';
        pos = pos + DIM.M;
    end
end

return;


% ----------------------
% This routine returns the vector of the time
% derivatives for the system.
function f = OC_f(t,x,U,FUN,PARAM,SIM,DIM,TRAJ);
f = zeros(DIM.N,1);

% get current control inputs
u = OC_get_u(t,U,SIM,DIM,TRAJ);

% call user-supplied routine to get time derivatives
f = feval(FUN.f,t,x,u,PARAM);

return;


% ----------------------
% This routine returns the current control input from the
% piecewise constant functional.
function u = OC_get_u(t,U,SIM,DIM,TRAJ);

u = zeros(DIM.M,1);

% find current interval
if(t < 0)
    interval = 1;
elseif(t < SIM.tH)
    interval = 1 + floor((t-SIM.t0)/SIM.dt);
else
    interval = SIM.NS;
end

% extract control input from the value for this
% interval
pos = (interval-1)*DIM.M;
u = U(pos+1:pos+DIM.M);

return;


% --------------------
% This routine sets up the constraints on the
% control inputs for the piecewise-constant
% trajectory.
function [A,b,A_eq,b_eq,LB,UB,nonlcon] = ...
    OC_constraints(SIM,DIM,FUN);

% get the constraint structures that apply to a single u
UCON = feval(FUN.constraint);

% copy into structures if necessary

% linear inequality constraints
if(size(UCON.A,1) == 0)  % empty structure
    A = [];
    b = [];
elseif(size(UCON.A,2) == DIM.M)
    NC = size(UCON.A,1);  % # of constraints per u
    A = zeros(SIM.NS*NC,SIM.NS*DIM.M);
    b = zeros(SIM.NS*NC,1);
    rpos = 0;  cpos = 0;
    for k=1:SIM.NS
        A(rpos+1:rpos+NC,cpos+1:cpos+DIM.M) = UCON.A;
        b(rpos+1:rpos+NC) = UCON.b;
        rpos = rpos + NC;  cpos = cpos + DIM.M;
    end    
else
    error('OC_constraints: A is not dimensioned properly');
end
    
% linear equality constraints
if(size(UCON.A_eq,1) == 0)  % empty structure
    A_eq = [];
    b_eq = [];
elseif(size(UCON.A_eq,2) == DIM.M)
    NC = size(UCON.A_eq,1);  % # of constraints per u
    A_eq = zeros(SIM.NS*NC,SIM.NS*DIM.M);
    b_eq = zeros(SIM.NS*NC,1);
    rpos = 0;  cpos = 0;
    for k=1:SIM.NS
        A_eq(rpos+1:rpos+NC,cpos+1:cpos+DIM.M) = UCON.A_eq;
        b_eq(rpos+1:rpos+NC) = UCON.b_eq;
        rpos = rpos + NC;  cpos = cpos + DIM.M;
    end    
else
    error('OC_constraints: A_eq is not dimensioned properly');
end

% set upper and lower bounds
LB = zeros(SIM.NS*DIM.M,1);  UB = zeros(SIM.NS*DIM.M,1);
pos = 0;
for k=1:SIM.NS
    LB(pos+1:pos+DIM.M) = UCON.LB;
    UB(pos+1:pos+DIM.M) = UCON.UB;
    pos = pos + DIM.M;
end

% we do not include any nonlinear input constraints
nonlcon = [];

return;


