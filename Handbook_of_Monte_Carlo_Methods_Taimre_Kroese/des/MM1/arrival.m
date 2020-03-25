%MM1/arrival.m
N_ev = N_ev + 1;
ev_list(N_ev,:) = [t - log(rand)/lambda, 1]; %schedule new arrival
if x == 0  % if queue is empty
    N_ev = N_ev + 1;
    ev_list(N_ev,:) = [t - log(rand)/mu, 2]; % schedule departure
end
x = x+1;
