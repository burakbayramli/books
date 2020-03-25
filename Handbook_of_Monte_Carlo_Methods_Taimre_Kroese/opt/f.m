function cost=f(r,R)
% For a given policy (r,R), return the simulated cost
c1 = 5; c2 = 500; c3 = 100; % Cost constants
T = 1000;
x = R; y = R; %x net inventory. y inventory position
xold = x;
t = 0; told=0;
ev_list = inf*ones(3,3); %Event list: first entry is time, 
                         %second type, third size of demand/order
totneg = 0; num_ord = 0;
ev_list(1,:) = [interarrival, 1,demandsize];%schedule first demand
N_ev = 1; % number of events scheduled
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
    N_ev = N_ev - 1;
    ev_list(1,:) = [inf,inf,inf];
    ev_list = sortrows(ev_list,1); % sort event list
    
    totneg = totneg + (xold < 0)*(t - told);
    xold = x; told =t;
end
frac_neg = totneg/t ;
freq_ord = num_ord/t;
cost = c1*R + c2*frac_neg + c3*freq_ord;  %cost per unit of time