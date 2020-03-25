%MM1/main.m
mu = 3; lambda = 2;
rho = lambda/mu;
T = 10000;
x = 0; xold = 0; % initialize current state and previous state
ev_list = inf*ones(3,2); % initialize event list
t = 0; told = 0; % initialize current and previous event time
tot = 0; %
ev_list(1,:) = [- log(rand)/lambda, 1]; %schedule the first arrival
N_ev = 1; 		  % number of scheduled events
while t < T
    t = ev_list(1,1);
    ev_type = ev_list(1,2);
    switch ev_type
        case 1
            arrival
        case 2
            departure
    end
    ev_list(1,:) = [inf,inf];
    N_ev = N_ev - 1;
    ev_list = sortrows(ev_list,1); % sort event list
    tot =tot + xold*(t - told);
    xold = x; told =t;
end
res = tot/t
exact = rho/(1-rho)

