%Repairman/failure.m
if (r > 0) %repairman available
    N_ev = N_ev + 1;
    %schedule repair
    ev_list(N_ev,:) = [t + repairtime(mach_num), 2,mach_num]; 
    r = r -1;
else
    repairq = [repairq,mach_num];
end
f = f+ 1;
