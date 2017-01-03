function demoMDPclean
%DEMOMDPCLEAN demo of a solving a grid MDP (no noise on transitions)
Gx = 10; Gy = 10;  % two dimensional grid size
S = Gx*Gy; % number of states on grid
st = reshape(1:S,Gx,Gy); % assign each grid point a state

A = 5;  % number of action (decision) states
[stay up down left right] = assign(1:A); % actions (decisions)
p = zeros(S,S,A); % initialise the transition p(xt|xtm,dtm) ie p(x(t)|x(t-1),d(t-1))

% make a deterministic transition matrix on a 2D grid:
for x = 1:Gx
	for y = 1:Gy
		p(st(x,y),st(x,y),stay)=1; % can stay in same state
		if validgridposition(x+1,y,Gx,Gy)
			p(st(x+1,y),st(x,y),right)=1;
		end
		if validgridposition(x-1,y,Gx,Gy)
			p(st(x-1,y),st(x,y),left)=1;
		end
		if validgridposition(x,y+1,Gx,Gy)
			p(st(x,y+1),st(x,y),up)=1;
		end
		if validgridposition(x,y-1,Gx,Gy)
			p(st(x,y-1),st(x,y),down)=1;
		end
	end
end
% define utilities
u = zeros(S,1);
Ngoals = 4; % number of `goal states' (ie states that have non-zero utiltiy)
r = randperm(S); u(r(1:Ngoals)) = 1; % choose random goal states
gam = 0.95; % discount factor
opts.plotprogress=1;
opts.maxiterations=30; opts.tol=0.001; % termination critria

% Value iteration
opts.method='value'; figure
val=MDPsolve(p,u,gam,opts); 
figure; bar3zcolor(reshape(val,Gx,Gy));  title('Value iteration value')

% Policy Iteration:
opts.method='policy'; figure
val=MDPsolve(p,u,gam,opts);
figure; bar3zcolor(reshape(val,Gx,Gy)); title('Policy iteration value')

% EM using deterministic policy
cut_off = 200; % finite time horizon
opts.maxiterations=1; opts.tol=0.00001;  opts.plotprogress=1; figure
% rewards can also be a function of the action u(x,a), so make a reward that accounts for this:
[value action] = MDPemDeterministicPolicy(p,repmat(u', A, 1), cut_off, S, A, gam,opts);
figure; bar3zcolor(reshape(value,Gx,Gy)); title('EM deterministic policy value')