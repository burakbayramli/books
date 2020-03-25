%Inventory/demand.m
demsize = ev_par;
x = x - demsize;
y = y - demsize;
if (y <s)   % if net inventory is under s limit
    N_ev = N_ev + 1;
    ev_list(N_ev,:) = [t + leadtime, 2,S-y]; % schedule order
    % size of order is S - x
    y = S;
end
N_ev = N_ev + 1;
ev_list(N_ev,:) = [t + interarrival, 1,demandsize]; %schedule demand
