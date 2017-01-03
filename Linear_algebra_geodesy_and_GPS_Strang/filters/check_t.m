function tt = check_t(t);
%CHECK_T accounting for beginning or end of week crossover

%Kai Borre 04-01-96
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 1998/10/28  $

half_week = 302400;
tt = t;
if t >  half_week
   tt = t-2*half_week; 
end
if t < -half_week
   tt = t+2*half_week; 
end
%%%%%%% end check_t.m  %%%%%%%%%%%%%%%%%
