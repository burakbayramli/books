%MM1/departure.m
x = x-1; % go out of queue
if x ~= 0
    N_ev = N_ev + 1;
    ev_list(N_ev,:) = [t - log(rand)/mu, 2]; % schedule departure
end
