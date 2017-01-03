function  [ATA,ATb] = normals(ATA,ATb,A,omc,var)
%NORMALS Accumulates the contribution of one	observation equation and
%        adds it to the coefficient matrix ATA and the right side ATb.
%	      The accumulated result is output under the same name.

%Kai Borre 09-16-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

ATb = ATb + A*omc/var;
ATA = ATA + A*A'/var;
%%%%%%% end normals.m  %%%%%%%%%%%%%
