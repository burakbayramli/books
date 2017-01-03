function [PARMIN,PARMAX,MYOPT] = SDE_library_optsetup(bigtheta)

% Optimization setup: load the default upper and lower limits for the
% parameters space and the default optimization options.
%
% usage: [PARMIN,PARMAX,MYOPT] = SDE_library_optsetup(bigtheta)
%
% IN:  bigtheta; complete structural parameter vector
% OUT: PARMIN: array of lower bounds for the complete structural parameter vector bigtheta
%      PARMAX: array of upper bounds for the complete structural parameter vector bigtheta
%      MYOPT: default optimization settings for the fminsearchbnd.m Nelder-Mead minimization algorithm

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

error(nargchk(1, 1, nargin));

PARMIN = zeros(1,length(bigtheta));
PARMAX = zeros(1,length(bigtheta));

for i=1:length(bigtheta)
    if sign(bigtheta(i))>0
       PARMIN(i) = -20 * bigtheta(i);
       PARMAX(i) = 20 * bigtheta(i);
    end
    if sign(bigtheta(i))==0
       PARMIN(i) = 20 ;
       PARMAX(i) = 20 ;
    end
    if sign(bigtheta(i))<0
       PARMAX(i) = -20 * bigtheta(i);
       PARMIN(i) = 20 * bigtheta(i);
    end
end

MYOPT = optimset('fminsearch');
MYOPT = optimset(MYOPT,'MaxFunEvals',20000,'MaxIter',5000, 'TolFun',1.e-4,'TolX',1.e-4,'Display','iter');    
        
        