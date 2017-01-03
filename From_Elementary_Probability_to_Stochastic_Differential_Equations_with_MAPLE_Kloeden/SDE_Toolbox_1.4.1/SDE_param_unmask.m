
function [bigtheta, thetaidx] = SDE_param_unmask(theta,PARMASK,PARBASE)
%
%         Returns the complete parameter set by inserting the running theta free subset parameter values 
%         into the saved PARBASE all parameter values. It also returns the positions of the theta parameters, just in case.
%
% usage: [bigtheta, thetaidx] = SDE_param_unmask(theta)
% IN:     theta (n,1); running theta free subset parameter
%         PARMASK; vector 0/1 specifying which parameter are free (1) and which one are not-free (0)
%         PARBASE; all saved parameter values
% OUT:    bigtheta (m,1); complete vector of parameters updating with the running theta free parameters
%         thetaidx (m,1); positions of all parameters 
%

% October 2007, Umberto Picchini
% January 2004, Andrea De Gaetano (BioMatLab IASI-CNR, www.biomatematica.it)

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


error(nargchk(3, 3, nargin));

% filter theta into bigtheta according to PARMASK:
bigtheta = PARBASE;
j=1;
for i=1:length(PARMASK)
   if (PARMASK(i) == 1)
      bigtheta(i) = theta(j);
      thetaidx(j) = i;
      j = j+1;
      if (j> length(theta))
         break
      end
   end
end

