%TandemQ/arrival.m
N_ev = N_ev + 1;
ev_list(N_ev,:) = [t + rand, 1]; %schedule new arrival
if x == 0  % if queue is empty
   N_ev = N_ev + 1;   
   ev_list(N_ev,:) = [t + rand, 2]; % schedule departure at queue 1
end
x = x+1;
