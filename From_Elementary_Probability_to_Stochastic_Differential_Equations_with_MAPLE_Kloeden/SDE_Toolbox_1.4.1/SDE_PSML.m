function negloglikelihood = SDE_PSML(theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED)

% Parametric Simulated Maximum Likelihood (SML) for the estimation of Markovian Ito SDE systems
% according to [1]-[2].
%
% IN:    theta; the array of free parameters
%      OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%                  It has starting simulation-time in first and ending simulation-time in last position. 
%                  Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%                  for the numerical integration (i=2,3,...)
%         TIME; the array of unique observation times
%         VRBL; the array of unique label-variables 
%         XOBS; the matrix-shaped observed data
%       NUMSIM; the number of desired simulations for the SDE numerical integration 
%      PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%      SDETYPE; the SDE definition: in this case it must be a 'Ito' SDE;
%      PARBASE; the same as bigtheta, provides parameters starting values for the optimization procedure 
%       PARMIN; array of lower bounds for the complete structural parameter vector bigtheta
%       PARMAX; array of upper bounds for the complete structural parameter vector bigtheta
%      PARMASK; an array containing ones in correspondence of the parameters in bigtheta to be estimated 
%               and zeros in correspondence of the parameters to be held fixed (constant); it has the same 
%               length of bigtheta. 
%   INTEGRATOR; the SDE fixed stepsize numerical integration method: in this case it MUST be 'EM' (Euler-Maruyama) 
%   NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%               type 'help randn' for details;
%
% References: 
% [1] A.R. Pedersen. "A new approach to maximum likelihood estimation for stochastic differential equations based on discrete observations". 
% Scandinavian Journal of Statistics, 22, 55-71, 1995.
% [2] M.W. Brandt and P. Santa-Clara. "Simulated likelihood estimation of diffusions with an application to exchange rate dynamics in incomplete
% markets". Journal of Financial Economics, 63, 161-210, 2002.

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
error(nargchk(15, 15, nargin));
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end
[n1,n2]=rat(NUMDEPVARS);
if(~isequal(n2,1) || isempty(NUMDEPVARS) || NUMDEPVARS <= 0)
    error('The number of variables NUMDEPVARS must be a positive integer');
end
if(strcmp(upper(SDETYPE),'ITO')==0)
    error('SDETYPE must be ''ITO''');
end
if(strcmp(upper(INTEGRATOR),'EM')==0)
    error('The parametric PSML method can be used only with the ''EM'' integration method');
end

bigtheta = SDE_param_unmask(theta,PARMASK,PARBASE); 

% check parameter values and refuse computation if outside limits (for ALL parameters)
for i=1:length(bigtheta)
   if ((bigtheta(i) < PARMIN(i)) || (bigtheta(i) > PARMAX(i)))
      negloglikelihood = 1e+32;
      return;
   end
end

n = length(unique(TIME));
density = zeros(n-1,NUMSIM);
transdensity = zeros(n-1,1);

% Use the Euler-Maruyama approximation, as described in [1]-[2]
density = SDE_PSML_euler(bigtheta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,NUMDEPVARS,SEED);

transdensity = sum(density,2)/NUMSIM;   
negloglikelihood = - sum(log(transdensity));



