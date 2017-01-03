function demoMDP
%DEMOMDP demo of solving Markov Decision Process on a grid
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
Ngoals = 4; % number of `goal states' (ie states that have non-zero utility)
r = randperm(S); u(r(1:Ngoals)) = 1; % choose random goal states
gam = 0.95; % discount factor

[xt xtm dtm]=assign(1:3); % assign the variables x(t), x(t-1), d(t-1) to some numbers

% define the transition potential: p(x(t)|x(t-1),d(t-1))
tranpot.variables=[xt xtm dtm]; tranpot.table=p;
% setup the value potential: v(x(t))
valpot.variables=xt; valpot.table=ones(S,1); % initial values

maxiterations=30; tol=0.001; % termination criteria
% Value Iteration:
oldvalue=valpot.table;
for valueloop=1:maxiterations
	valueloop
	tmppot = maxpot(sumpot(multpots([tranpot valpot]),xt),dtm);
	valpot.table = u + gam*tmppot.table; % Bellman's recursion
	if mean(abs(valpot.table-oldvalue))<tol; break; end % stop if converged
	oldvalue = valpot.table;
	imagesc(reshape(valpot.table,Gx,Gy)); colorbar; drawnow
end
figure; bar3zcolor(reshape(valpot.table,Gx,Gy));

% Policy Iteration:
valpot.table=ones(S,1); % initial values
oldvalue=valpot.table;
figure;
for policyloop=1:maxiterations
	policyloop
	% Policy evaluation: get the optimal decisions as a function of the state:
	[tmppot dstar] = maxpot(sumpot(multpots([tranpot valpot]),xt),dtm);
	for x1=1:S
		for x2=1:S
			pdstar(x1,x2) = p(x2,x1,dstar(x1));
		end
	end
	valpot.table = (eye(S)-gam*pdstar)\u;
	if mean(abs(valpot.table-oldvalue))<tol; break; end % stop if converged
	oldvalue=valpot.table;
	imagesc(reshape(valpot.table,Gx,Gy)); colorbar; drawnow
end
figure; bar3zcolor(reshape(valpot.table,Gx,Gy));

cut_off = 100; % finite time horizon
opts.maxiterations=3; opts.tol=0.00001;  opts.plotprogress=1;
% rewards can also be a function of the action u(x,a), so make a reward
% that accounts for this:
[value action] = MDPemDeterministicPolicy(p,repmat(u', A, 1), cut_off, S, A, gam,opts);
figure; imagesc(reshape(value,Gx,Gy)); colorbar; drawnow; title('EM deterministic policy value')
figure; bar3zcolor(reshape(valpot.table,Gx,Gy)); title('EM deterministic policy value')
