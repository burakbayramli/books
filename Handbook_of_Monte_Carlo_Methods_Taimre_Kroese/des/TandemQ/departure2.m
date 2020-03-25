%TandemQ/departure2.m
y = y-1; % go out of second queue
if y ~= 0
  N_ev = N_ev + 1;   
  ev_list(N_ev,:) = [t + rand, 3]; % schedule departure at queue 2
end
