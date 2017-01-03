%COMPTIME Reads receiver clock offset from a binary Ashtech observation
%	  file and plots it.
%	  See also b_clock

%Kai Borre 03-22-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

global outm

v_light = 299792458;
b_clock('b0005a94.076')
outm = outm(:,[1]);
Outm = outm/(v_light*1.e-9);
plot(Outm)
%%%%%%%%%%%%%%%%% end comptime.m %%%%%%%%%%%%%%%%%%%
