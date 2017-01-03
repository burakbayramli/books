function xhatpredict = SDE_predict(bigtheta,PROBLEM,SDETYPE,OWNTIME,TIME,NUMDEPVARS,NUMSIM,VRBL,INTEGRATOR,SEED)

% Linear interpolation of simulated trajectories: useful to generate data
%
% usage: xhatpredict = SDE_predict(bigtheta,PROBLEM,SDETYPE,OWNTIME,TIME,NUMDEPVARS,NUMSIM,VRBL,INTEGRATOR,SEED)
%
% IN:    bigtheta; complete vector of structural model parameters
%         PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%         SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%         OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%                  It has starting simulation-time in first and ending simulation-time in last position. 
%                  Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%                  for the numerical intregration (i=2,3,...)
%            TIME; the array of unique observation times
%      NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%          NUMSIM; the number of desired simulations for the SDE numerical integration 
%            VRBL; the array of unique label-variables 
%      INTEGRATOR; the SDE fixed stepsize numerical integration method: can be 'EM' (Euler-Maruyama) or 'Mil' (Milstein)
%            SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%                  type 'help randn' for details;
% OUT:       xhat; the SDE linearly interpolated solution

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

% input check
error(nargchk(10, 10, nargin));
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end
[n1,n2]=rat(NUMDEPVARS);
if(~isequal(n2,1) || isempty(NUMDEPVARS) || NUMDEPVARS <= 0)
    error('The number of variables NUMDEPVARS must be a positive integer');
end
if(strcmp(upper(INTEGRATOR),'EM')==0 && strcmp(upper(INTEGRATOR),'MIL')==0)
    error('INTEGRATOR must be ''EM'' (Euler-Maruyama) or ''MIL'' (Milstein)');
end

xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED);

n = length(unique(TIME));
xhatpredict = zeros(n,NUMSIM*length(unique(VRBL)));
% for i=1:n
%     xhatpredict(i,:) = interp1(OWNTIME,xhat(:,VRBL(i):NUMDEPVARS:end),TIME(i),'linear');
% end

vrbl = unique(VRBL);
col = 1;
for j=vrbl(1):vrbl(end)
    xhatpredict(:,col:length(vrbl):end) = interp1(OWNTIME,xhat(:,j:NUMDEPVARS:end),unique(TIME)','linear');
    col = col + 1;
end
