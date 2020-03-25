%TandemQ/departure1.m
x = x-1; % go out of first queue
if x ~= 0
  N_ev = N_ev + 1;   
  ev_list(N_ev,:) = [t + rand, 2]; % schedule departure at queue 1
end
if y == 0
  N_ev = N_ev + 1; 
  ev_list(N_ev,:) = [t + rand, 3]; % schedule departure at queue 2
end
y = y + 1; % go in second queue
