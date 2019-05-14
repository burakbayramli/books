% ======================================================================
% This function implements the value iteration algorithm for the brick.
% The tasks are:
% 1) lqr regulator task w/ discrete actions
% 2) lqr regulator task w/ continuous actions
% 3) limit cycle task
% It returns the optimal value function J and policy PI.
% - Rick
% ======================================================================

function [J,PI] = brick_vi
global problem dt pfig sfig;
pfig = 11; % policy figure handle
sfig = 25; % simulation figure handle

% === DEFINE THE PROBLEM TO RUN HERE ===
% 'mtime' - minimum time problem
% 'dlqr' - lqr regulator task w/ discrete actions
% 'clqr' - lqr regulator task w/ continuous actions
% 'cycle' - limit cycle trajectory task
problem = 'mtime';

% define the mesh points
q_bins = linspace(-5,5,25);
qdot_bins = linspace(-10,10,25);
if (strcmp(problem,'mtime'))
    q_bins = linspace(-2,2,65);
    qdot_bins = linspace(-2,2,65);
end

% dynamics dt
dt= 1e-2;

% create the mesh
[q qdot] = ndgrid(q_bins,qdot_bins);
s = [reshape(q,1,numel(q)); reshape(qdot,1,numel(qdot))];
ns = size(s,2);

% code for discrete actions
if (~strcmp(problem,'clqr'))
    if (strcmp(problem,'mtime'))
        a = [-1 1];
    else
        a = linspace(-5,5,11);
    end
    na = size(a,2);
    % generate all possible state and action pairs
    S = repmat(s,1,na); % repeat s na times
    A = reshape(repmat(a,ns,1),1,ns*na); % repeat a ns times

    % compute the one-step dynamics
    Sn = S + dynamics(S,A).*dt;

    % Compute the transition matrix
    disp('Computing Transition Matrix...');
    [Pi,P] = volumetric_interp(s,Sn,q_bins,qdot_bins);

    % Compute the one-step cost
    C = reshape(cost(S,A),ns,na);
end

% Setup value iteration
J = zeros(ns,1); % arbitrary initialization
converged  = 0.1; % value converged threshold
% don't discount min-time problem
if (strcmp(problem,'mtime'))
    gamma = 1;
else
    gamma = 0.99; % discount factor
end
iter = 1; err = 1e6;

% now iterate the value estimate

% THIS CODE IS FOR DISCRETE ACTIONS
if (~strcmp(problem,'clqr'))
    while (err > converged)
        [Jnew, PI] = min(C + gamma*reshape(sum(P.*J(Pi),1),ns,na),[],2);
        err = max(abs(Jnew-J));
        disp(['iteration = ',num2str(iter),' ; max_err = ',num2str(err)]);
        J = Jnew; iter = iter+1;
        vi_plot(J,a(PI),q_bins,qdot_bins);
    end
    PI = a(PI)'; 
else
    % THIS CODE IS FOR CONTINUOUS ACTIONS
    while (err > converged)
        % compute the gradient
        dJdx = get_grad(J,s,s,q_bins,qdot_bins);
        [Q,R] = get_QR;
        
        % compute the optimal actions
        % note: need continuous time R
        uopt = -(dt/R)*dJdx(2,:);
        
        % compute the one step cost using uopt
        C = cost(s,uopt);
        
        % compute the cost-to-go of our next state
        % using action uopt
        Sn = s + dynamics(s,uopt).*dt;
        [Pi,P] = volumetric_interp(s,Sn,q_bins,qdot_bins);
        Jnext = gamma*sum(P.*J(Pi),1);
        
        % update the value function
        Jnew = (C + Jnext)';
        err = max(abs(Jnew-J));
        disp(['iteration = ',num2str(iter),' ; max_err = ',num2str(err)]);
        J = Jnew; iter = iter+1;
        vi_plot(J,uopt,q_bins,qdot_bins);
    end
end

disp('Value Estimate converged!');
subplot(2,1,1);xlabel('q'); ylabel('q_{dot}'); title('u'); colorbar;
subplot(2,1,2);xlabel('q'); ylabel('q_{dot}'); title('Value function'); colorbar;
% plot the analytical min_time policy
if (strcmp(problem,'mtime'))
    figure(pfig);subplot(2,1,1); hold on;
    plot(q_bins, -sign(q_bins).*sqrt(2*sign(q_bins).*q_bins),'c','LineWidth', 2); hold off;
end

disp('Press Enter to simulate...');pause;
% simulate a trajectory
T = 20; disp_dts = 5;
xtraj = zeros(2,T/dt); x = [-1 -1]';
utraj = zeros(1,size(xtraj,2));
for i=1:T/dt
    xtraj(:,i) = x;
    
    if (~strcmp(problem,'clqr'))
        [ind,coef] = volumetric_interp(s,x,q_bins,qdot_bins);
        u = sum(coef.*PI(ind),1);
    else
        % cont. actions
        dJdx = get_grad(J,x,s,q_bins,qdot_bins);
        [Q,R] = get_QR;
        u = -(dt/R)*dJdx(2);
    end
    if (mod(i,disp_dts)==0)
        draw((i-1)*dt,x);
    end
    utraj(i) = u;
    x = x + dynamics(x,u).*dt;
end

% plot the trajectory
figure(pfig);
subplot(2,1,1); hold on; plot(xtraj(1,:),xtraj(2,:),'w'); hold off;
subplot(2,1,2); hold on; plot(xtraj(1,:),xtraj(2,:),'w'); hold off;

figure(20);plot(dt.*(1:length(utraj)),utraj); 
title('u trajectory'); xlabel('time'); ylabel('u');

end % end of pend_vi

% ==========================================================
% This function performs volumetric interpolation on the
% state(s) Sn, returning the box indices(Pi) and weights(P)
% (a.k.a. transition probabilities)
% ==========================================================
function [Pi,P] = volumetric_interp(s,Sn,q_bins,qdot_bins)
ns = size(Sn,2);
Pi = zeros(4,ns);
P = Pi;

% impose limits and wrapping on the state
Sn = normalize(Sn,q_bins,qdot_bins);

% compute each transition individualy
for i=1:ns
    if (mod(i,4e3)==0)
        disp([num2str((i/ns)*1e2),'% done']);
    end
    % lower left corner
    ind_q = max([find(q_bins <= Sn(1,i),1,'last') 1]);
    ind_qdot = max([find(qdot_bins <= Sn(2,i),1,'last') 1]);
    
    offset = [0 0;1 0;0 1;1 1];
    if (ind_q == length(q_bins))
        offset(:,1) = -offset(:,1);
    end
    if (ind_qdot == length(qdot_bins))
        offset(:,2) = -offset(:,2);
    end
    % compute the total area of the containing box
    totl_area = abs(q_bins(ind_q+offset(2,1))-q_bins(ind_q))*...
                abs(qdot_bins(ind_qdot+offset(3,2))-qdot_bins(ind_qdot));
    % compute the four corner indices and weights
    for j=1:4
        state = [q_bins(ind_q+offset(j,1)); qdot_bins(ind_qdot+offset(j,2))];
        Pi(j,i) = find(sum(abs(s-repmat(state,1,size(s,2))),1)==0);
        P(5-j,i) = (abs(Sn(1,i)-q_bins(ind_q+offset(j,1)))*abs(Sn(2,i)-qdot_bins(ind_qdot+offset(j,2))))/totl_area;
    end
end
end

% =============================================================
% This function returns the gradients of the value function
% J (a column vector) for a given state x = [q qdot]'. 
% The inputs are:
% J: the value function
% x: the state you want the gradients for
% s: the mesh over the state space
% [q_bins,qdot_bins]: the bins used to create the mesh for s
% =============================================================
function dJdx = get_grad(J,x,s,q_bins,qdot_bins)

nx = size(x,2);
dJdx = zeros(size(x));

% impose limits and wrapping on the state
x = normalize(x,q_bins,qdot_bins);

% compute each gradient individualy
% (a vectorized form would be more efficient)
for i=1:nx
    if (mod(i,4e3)==0)
        disp([num2str((i/nx)*1e2),'% done']);
    end
    [ind,coef] = volumetric_interp(s,x(:,i),q_bins,qdot_bins);
    Jcor = J(ind); % the value function at the corners
    q = s(1,ind); qdot = s(2,ind); % the state at the corners
    dJdq = (Jcor(2)-Jcor(1))./(q(2)-q(1));
    dJdqdot = (Jcor(3)-Jcor(1))./(qdot(3)-qdot(1));
    dJdx(:,i) = [dJdq; dJdqdot];
end
end

% =============================================================
% This function defines the continuous dynamics of the brick
% =============================================================
function xdot = dynamics(x,u)
global problem;

xdot = [x(2,:); u];

% for min-time problem, stay at the origin
% i.e. no bang-bang
if (strcmp(problem,'mtime'));
    ind = sum(abs(x),1) == 0;
    xdot(:,ind) = repmat([0 0]',1,size(x(:,ind),2));
end

end

% =============================================================
% This function defines the instantaneous cost (i.e. g(x,u))
% =============================================================
function C = cost(X,u)
global problem;

switch (problem)
    case 'mtime'
        C = double(sum(abs(X),1) > 0);
        return;
    case {'dlqr','clqr'}
        Xerr = X;
    case 'cycle'
        % limit cycle brick trajectory
        Xdes = repmat([0 5]',1,size(X,2));

        X(2,:) = abs(X(2,:));
        Xerr = X - Xdes;
end

% get the cost matrices
[Q,R] = get_QR;

% implement a quadratic cost
C = dot(Xerr,0.5*Q*Xerr) + 0.5*u.^2.*R;
end

% =========================================================
% This just makes the cost matrices accessible to the rest
% of the script.
% =========================================================
function [Q,R] = get_QR
global problem dt;

switch(problem)
    case {'dlqr','clqr'}
        Q = diag([10 1]).*dt;
        R = .1.*dt;
    case 'cycle'
        Q = diag([1 1]);
        R = .1;
end

end

% ==============================================================
% This function imposes limits on the state
% ==============================================================
function s = normalize(s,q_bins,qdot_bins)
% impose limits
N = size(s,2);
smax = repmat([q_bins(end);qdot_bins(end)],1,N); ind = s>smax;
s(ind) = smax(ind);
smin = repmat([q_bins(1);qdot_bins(1)],1,N); ind = s<smin;
s(ind) = smin(ind);
end

% ===============================================================
% This function plots the value function and policy
%================================================================
function vi_plot(J,PI,q_bins,qdot_bins)
global pfig;
figure(pfig);
n1 = size(q_bins,2); n2 = size(qdot_bins,2);
subplot(2,1,1);imagesc(q_bins,qdot_bins,reshape(PI,n1,n2)'); axis xy;
subplot(2,1,2);imagesc(q_bins,qdot_bins,reshape(J,n1,n2)'); axis xy;
drawnow;
end

% ==============================================================
% This is the draw function.
% ==============================================================
function draw(t,x)

global sfig;
persistent hFig blockx blocky;

if (isempty(hFig))
  hFig = figure(sfig);
  set(hFig,'DoubleBuffer','on');
  blockx = [-1, -1, 1, 1, -1];
  blocky = [0, 0.5, 0.5, 0, 0];
end

figure(hFig);
clf;

% draw the mass
brickcolor=[.75 .6 .5];
fill(blockx+repmat(x(1),1,5),blocky,brickcolor);
hold on

faintline=[.6 .8 .65]*1.1;
plot(min(blockx)+[0 0],[-5 5],'k:','Color',faintline);
plot(max(blockx)+[0 0],[-5 5],'k:','Color',faintline);

% draw the ground
line([-5, 5], [0, 0],'Color',[.3 .5 1],'LineWidth',1);
axis([-5 5 -1 2]);
%grid on
axis equal;
title(['t = ', num2str(t)]);

drawnow;
end