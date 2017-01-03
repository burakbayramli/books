function tt = check_t(t);
% CHECK_T repairs over- and underflow of GPS time

%Kai Borre 04-01-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/23  $

half_week = 302400;
tt = t;
if t >  half_week, tt = t-2*half_week; end
if t < -half_week, tt = t+2*half_week; end
%%%%%%% end check_t.m  %%%%%%%%%%%%%%%%%
