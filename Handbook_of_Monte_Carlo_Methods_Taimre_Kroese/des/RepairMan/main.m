%Repairman/main.m
global alpha
T = 1000;
mach_num = 0; repairq = [];
nrep = 4; nmach = 8; %number of repairmen and machines
alpha = [1,2,3,4,5,6,7,8];
r = nrep; %the number of repairmen available 
f = 0; %the number of machines failed
%rr= r; ff=f; tt=0; %save the history (needed for plotting)
tot_util_rep = 0; 
tot_util_mach = 0; 
rold = r; fold = f; told = 0;

ev_list = inf*ones(9,3); %event time, type, machine number
t = 0;
for i=1:nmach
    ev_list(i,:) = [lifetime(i), 1,i]; %schedule the failures 
end
ev_list = sortrows(ev_list,1); % sort event list
N_ev = nmach;
while t < T
    t = ev_list(1,1);
    %tt=[tt,t];
    ev_type = ev_list(1,2);
    mach_num = ev_list(1,3);
    switch ev_type
        case 1
            failure
        case 2
            repair
    end
    N_ev = N_ev - 1;
    ev_list(1,:) = [inf,inf,inf];
    ev_list = sortrows(ev_list,1); % sort event list
    tot_util_rep = tot_util_rep + (nrep - rold)*(t - told);
    tot_util_mach = tot_util_mach + (nmach - fold)*(t - told);
    rold=r; told=t; fold =f;
 %   rr=[rr,r];ff=[ff,f];
end
fprintf('repair util. = %g, machine util. = %g\n', ...
         tot_util_rep/t, tot_util_mach/t);
%plottrace
