function  accum0(ATA_to_add, ATb_to_add)
%ACCUM0 Accumulates the contribution of observations from
%	     one epoch. The result is output under the same name.

%Kai Borre 03-24-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/23 $

global ATA ATb
ATA = ATA+ATA_to_add;
ATb = ATb+ATb_to_add;
%%%%%%% end accum0.m  %%%%%%%%%%%%%
