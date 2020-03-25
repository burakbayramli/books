%TandemQ/main.m
T = 5000;
totevents = 0;
x = 0; y = 0; yold =0;
s2busy = 0; % total time server 2 is busy
% xx=x; yy=y;
tt=0;
ev_list = inf*ones(4,2);
t = 0; told = 0;
ev_list(1,:) = [rand, 1]; %schedule the first arrival
N_ev = 1;                 % number of events
while t < T
    totevents = totevents+1;
    t = ev_list(1,1);
    %tt=[tt,t];
    ev_type = ev_list(1,2);
    switch ev_type
        case 1
            arrival
        case 2
            departure1
        case 3
            departure2
    end
    ev_list(1,:) = [inf,inf];
    N_ev = N_ev - 1;
    ev_list = sortrows(ev_list,1); % sort event list
    % xx=[xx,x]; yy=[yy,y];
    s2busy = s2busy + (yold > 0)*(t - told);
    yold = y; told=t;
end
% plottraces;
res = s2busy/t
totevents
