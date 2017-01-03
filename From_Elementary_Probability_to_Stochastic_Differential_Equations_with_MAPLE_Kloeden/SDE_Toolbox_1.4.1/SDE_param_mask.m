function [theta, thetaidx, thetamin, thetamax] = SDE_param_mask(bigtheta,PARMASK,PARMIN,PARMAX)

%          Returns the vector of free parameters from the complete vector of
%          parameters bigtheta
%
% usage : [theta, thetaidx, thetanames, thetamin, thetamax] = SDE_param_mask(bigtheta)
% IN:      bigtheta (m,1); vector of all parameters
%          PARMASK; vector 0/1 specifying which parameters are free (1) and which ones are not free (0)
%          PARMAX, PARMIN; the parameter max and min values 
% OUT:     theta (n,1); vector of free parameters
%          thetaidx (n,1); positions of the free parameters in the bigtheta vector
%          thetamin (n,1); minimum of the free parameters
%          thetamax (n,1); maximum of the free parameters

% October 2007, Umberto Picchini
% October 2001, Andrea De Gaetano (BioMatLab IASI-CNR, www.biomatematica.it)

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


error(nargchk(4, 4, nargin));

npar = length(PARMASK);

if (npar ~= length(bigtheta))
   error('incompatible dimensions of PARMASK and bigtheta');
end

n=0;
for(i=1:npar)
   if PARMASK(i) == 1
      n=n+1;
   end
end

theta      = zeros(n,1);
thetaidx   = zeros(n,1);
thetamin   = zeros(n,1);
thetamax   = zeros(n,1);

j=1;
for i=1:npar
   if(PARMASK(i) == 1)
      theta(j)        = bigtheta(i);
      thetaidx(j)     = i;
      thetamin(j)     = PARMIN(i);
      thetamax(j)     = PARMAX(i);
      j=j+1;
   end
end

