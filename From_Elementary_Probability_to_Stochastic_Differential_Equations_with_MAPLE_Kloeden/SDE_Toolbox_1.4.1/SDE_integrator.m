function xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED)

% Numerical integration of SDE systems
%
% usage: xhat = SDE_integrator(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,INTEGRATOR,SEED)
%
% IN:     bigtheta; complete vector of structural model parameters
%         PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%         OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%                  It has starting simulation-time in first and ending simulation-time in last position. 
%                  Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%                  for the numerical intregration (i=2,3,...)
%         NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         NUMSIM; the number of desired simulations for the SDE numerical integration 
%         SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%         INTEGRATOR; the SDE fixed stepsize numerical integration method: can be 'EM' (Euler-Maruyama) or 'Mil' (Milstein)
%         SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%               type 'help randn' for details;
% OUTPUT: xhat; the SDE approximated solution according to the chosen INTEGRATOR

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
error(nargchk(8, 8, nargin));
if(strcmp(upper(SDETYPE),'ITO')==0 && strcmp(upper(SDETYPE),'STRAT')==0)
    error('SDETYPE must be ''ITO'' or ''STRAT'' (Stratonovich)');
end
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end
[n1,n2]=rat(NUMDEPVARS);
if(~isequal(n2,1) || isempty(NUMDEPVARS) || NUMDEPVARS <= 0)
    error('The number of variables NUMDEPVARS must be a positive integer');
end

switch upper(INTEGRATOR) 
case 'EM'  % the Euler-Maruyama integration scheme
      xhat = SDE_euler(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,SEED);                % the Euler-Maruyama solution of the Ito SDE  
case 'MIL'  % the Milstein integration scheme
      xhat = SDE_milstein(bigtheta,PROBLEM,OWNTIME,NUMDEPVARS,NUMSIM,SDETYPE,SEED);            % the Milstein solution of the Ito or Stratonovich SDE 
    otherwise
        error('INTEGRATOR must be ''EM'' (Euler-Maruyama) or ''MIL'' (Milstein)');
end  