%Inventory/main.m
s = 10; S = 40;
T = 200000;
x = S; y = S; %x net inventory. y inventory position
xold = x;
t = 0; told=0;
% xx = x; yy=y; tt=0;  %uncomment for plotting
ev_list = inf*ones(3,3); %entries: (time, type, order-size)
totneg = 0; num_ord = 0;
ev_list(1,:) = [interarrival, 1,demandsize]; %schedule the first demand
N_ev = 1;                 % number of events scheduled
while t < T
    t = ev_list(1,1);
    ev_type = ev_list(1,2);
    ev_par = ev_list(1,3);
    switch ev_type
        case 1
            demand;
        case 2
            order;
    end
    % tt=[tt,t]; xx=[xx,x];yy=[yy,y]; %uncomment for plotting
    N_ev = N_ev - 1;
    ev_list(1,:) = [inf,inf,inf];
    ev_list = sortrows(ev_list,1); % sort event list
    
    totneg = totneg + (xold < 0)*(t - told);
    xold = x; told =t;
end
frac_neg = totneg/t 
freq_ord = num_ord/t
c1 = 5; c2 = 500; c3 = 100;
cost = c1*S + c2*frac_neg + c3*freq_ord  %cost per unit of time
% plottrace %uncomment for plotting
