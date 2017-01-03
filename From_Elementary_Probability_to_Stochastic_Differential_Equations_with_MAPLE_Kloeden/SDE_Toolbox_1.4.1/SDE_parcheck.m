function ok = SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE)

% Checks that the free parameters are acceptable
%
% usage:  ok = SDE_parcheck(theta,PARMIN,PARMAX)  
%
% IN:     theta     the vector of free parameters
%         PARMIN:   array of lower bounds for the complete structural parameter vector bigtheta
%         PARMAX:   array of upper bounds for the complete structural parameter vector bigtheta
%         PARMASK; an array containing ones in correspondence of the parameters in bigtheta to be estimated 
%                  and zeros in correspondence of the parameters to be held fixed (constant); it has the same 
%                  length of bigtheta.
%         PARBASE: the same as bigtheta, provides parameters starting values for the optimization procedure
% OUT:    ok        flags 0 if OK, 1  if problems

% November 2007, Umberto Picchini
% October 2005, Andrea De Gaetano (BioMatLab IASI-CNR, www.biomatematica.it)

% Copyright (C) 2007, Umberto Picchini  
% umberto.picchini@biomatematica.it
% http://www.biomatematica.it/Pages/Picchini.html
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.


bigtheta = SDE_param_unmask(theta,PARMASK,PARBASE);
ok = 0;
if sum((bigtheta < PARMIN) | (bigtheta > PARMAX)) > 0
    ok = 1;
end




