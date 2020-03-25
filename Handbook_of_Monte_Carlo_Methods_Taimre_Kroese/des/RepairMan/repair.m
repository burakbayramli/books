%Repairman/repair.m
f = f - 1; % one less failed
sq = size(repairq,2);
if (sq > 0) %still one in the queue
    N_ev = N_ev + 1;
    % schedule next repair
    ev_list(N_ev,:) = [t + repairtime(mach_num), 2,repairq(1)]; 
    repairq = repairq(2:sq); %remove machine
else
    r = r+1;
end
N_ev = N_ev + 1;
% schedule failure of current machine
ev_list(N_ev,:) = [t + lifetime(mach_num), 1,mach_num]; 
