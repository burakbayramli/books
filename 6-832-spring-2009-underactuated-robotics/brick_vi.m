% ======================================================================
% This function implements the value iteration algorithm for the brick.
% It returns the optimal value function J and policy PI.
% - Rick
%
% You need to implement:
% 1) lqr regulator task w/ discrete actions
% 3) oscillating brick task
% 2) lqr regulator task w/ continuous actions
% ======================================================================

function [J,PI] = brick_vi
global dt;

% define the mesh points as a row vector (YOU FILL THIS IN)
q_bins = [ ];   
qdot_bins = [];
if (isempty(q_bins) || isempty(qdot_bins)) error('you need to define the mesh'); end

% The discrete actions, defined as a row vector (FOR PARTS a-c)
% NOTE: PART d REQUIRES THE CONTINUOUS ACTION IMPLEMENTATION
a = [];
if (isempty(a)) error('you need to define the action set'); end

% dynamics dt
dt = 1e-2;

% create the mesh
[q qdot] = ndgrid(q_bins,qdot_bins);
s = [reshape(q,1,numel(q)); reshape(qdot,1,numel(qdot))];
ns = size(s,2); na = size(a,2);

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

% Setup value iteration (you shouldn't need to change this)
J = zeros(ns,1); % arbitrary initialization
converged  = 0.1; % value converged threshold
gamma = 0.99; % discount factor
iter = 1; err = 1e6;

% Iterate the value estimate

% USE THE VALUE ITERATION UPDATE GIVEN HERE FOR PARTS a-c. FOR PART d, YOU
% WILL NEED TO MODIFY THIS UPDATE TO USE CONTINUOUS ACTIONS.
while (err > converged)
    [Jnew, PI] = min(C + gamma*reshape(sum(P.*J(Pi),1),ns,na),[],2);
    err = max(abs(Jnew-J));
    disp(['iteration = ',num2str(iter),' ; max_err = ',num2str(err)]);
    J = Jnew; iter = iter+1;
    vi_plot(J,a(PI),q_bins,qdot_bins);
end

% === HINT ============================================================
% For implementing cont. actions, you need to do the following:
% 1) compute the gradients of the J wrt x using the function 'get_grad'
% 2) get the R matrix using 'get_QR' (remember to divide the discrete R by dt)
% 3) compute the optimal action using the gradients of J
% 4) compute Jnew by computing the one-step-cost and the cost-to-go for the
%    next states
% =====================================================================

disp('Value Estimate converged!');
subplot(2,1,1);xlabel('q'); ylabel('q_{dot}'); title('u'); colorbar;
subplot(2,1,2);xlabel('q'); ylabel('q_{dot}'); title('Value function'); colorbar;

disp('Press Enter to simulate...');pause;
% simulate a trajectory
PI = a(PI)'; T = 20; disp_dts = 5;
xtraj = zeros(2,T/dt); x = [-3 0]';
for i=1:T/dt
    xtraj(:,i) = x;    
    
    % FOR PART d, MAKE USE OF CONTINOUS ACTIONS INSTEAD
    % OF THE INTERPOLATED DISCRETE ACTIONS GIVEN HERE.
    [ind,coef] = volumetric_interp(s,x,q_bins,qdot_bins);
    u = sum(coef.*PI(ind),1);
    
% === HINT ============================================================
% For implementing cont. actions, you need to follow a similar procedure
% to the one for computing Jnew above
% =====================================================================    
    
    if (mod(i,disp_dts)==0)
        draw((i-1)*dt,x);
    end
    x = x + dynamics(x,u).*dt;
end

end % end of brick_vi

% ==========================================================
% This function performs volumetric interpolation on the
% state(s) Sn, returning the box indices(Pi) and weights(P)
% (a.k.a. transition probabilities)
% The input is:
% s: the mesh
% Sn: the states to interpolate for
% [q-bins, qdot_bins]: the bins used to create the mesh
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
xdot = [x(2,:); u];
end

% ===================================================================
% This function defines the instantaneous cost (i.e. g(x,u))
% YOU SHOULD FILL THIS IN. 
% ===================================================================
function C = cost(X,u)
 C = 0;
 error('you need to define the cost function');  % remove this line once you've done it
end

% =========================================================
% This just makes the cost matrices accessible to the rest
% of the script (e.g. you need R for the cont. action 
% implementation).
% =========================================================
function [Q,R] = get_QR
global dt;

% DEFINE THESE COST MATRICES CORRECTLY.
Q = diag([0 0]).*dt; % <== the dt just discretizes
R = 0*dt;
error('you need to define the cost matrices'); % remove this when you've done it

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
figure(10); n1 = size(q_bins,2); n2 = size(qdot_bins,2);
subplot(2,1,1);imagesc(q_bins,qdot_bins,reshape(PI,n1,n2)'); axis xy;
subplot(2,1,2);imagesc(q_bins,qdot_bins,reshape(J,n1,n2)'); axis xy;
drawnow;
end

% ==============================================================
% This is the draw function.
% ==============================================================
function draw(t,x)

persistent hFig blockx blocky;

if (isempty(hFig))
  hFig = figure(25);
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