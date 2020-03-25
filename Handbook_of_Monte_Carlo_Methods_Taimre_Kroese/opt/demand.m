%demand.m
demsize = ev_par;
x = x - demsize;
y = y - demsize;
if (y <r)   % if net inventory is under r limit
    N_ev = N_ev + 1;
    ev_list(N_ev,:) = [t + leadtime, 2,R-y]; % schedule order
    % size of order is R - x
    y = R;
end
N_ev = N_ev + 1;
ev_list(N_ev,:) = [t + interarrival, 1,demandsize]; %schedule demand
