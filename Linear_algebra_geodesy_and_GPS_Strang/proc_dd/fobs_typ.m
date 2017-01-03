function i = fobs_typ(Obs_types, type)
%FOBS_TYP Returns column i of the observation matrix
%   	    which contains observation type "type"

%Kai Borre 03-21-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

s = findstr(Obs_types, type);
i = (s+1)/2;
%%%%%%%%%%%%%% end fobs_typ.m  %%%%%%%%%%%%%%%%%%%%%%%%5
