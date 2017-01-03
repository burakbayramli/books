function i = fobs_typ(Obs_types, type)
% FOBS_TYP  Returns column i of the observation matrix
%	    which contains observation type "type"

%  Written by Kai Borre
%  March 21, 1997

       s = findstr(Obs_types, type);
       i = (s+1)/2;

%%%%%%%%%%%%%% end fobs_typ.m  %%%%%%%%%%%%%%%%%%%%%%%%5
