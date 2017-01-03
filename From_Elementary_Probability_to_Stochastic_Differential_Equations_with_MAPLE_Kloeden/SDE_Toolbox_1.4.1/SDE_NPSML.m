function negloglikelihood = SDE_NPSML(theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED)

% Non-Parametric Simulated Maximum Likelihood for Markovian Ito and Stratonovich SDEs according to [1]. 
%
% This implementation onsiders only the case where the system variables of the process are 
% pairwise independent (not just uncorrelated). In this case the multidimensional kernel density 
% estimator can be expressed as the product of the kernels of each variable (see [2] section 6.3.1). 
%
% IN:    theta; the array of free parameters
%      OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%                  It has starting simulation-time in first and ending simulation-time in last position. 
%                  Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%                  for the numerical intregration (i=2,3,...)
%         TIME; the array of unique observation times
%         VRBL; the array of unique label-variables 
%         XOBS; the matrix-shaped observed data
%       NUMSIM; the number of desired simulations for the SDE numerical integration 
%      PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%      SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%      PARBASE; the same as bigtheta, provides parameters starting values for the optimization procedure 
%       PARMIN; array of lower bounds for the complete structural parameter vector bigtheta
%       PARMAX; array of upper bounds for the complete structural parameter vector bigtheta
%      PARMASK; an array containing ones in correspondence of the parameters in bigtheta to be estimated 
%               and zeros in correspondence of the parameters to be held fixed (constant); it has the same 
%               length of bigtheta. 
%   INTEGRATOR; the SDE fixed stepsize numerical integration method: can be 'EM' (Euler-Maruyama) or 'Mil' (Milstein)
%   NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%               type 'help randn' for details;
%
% References: 
% [1] A.S. Hurn, K.A. Lindsay and V.L. Martin "On the effficacy of simulated maximum likelihood for estimating the parameters of stochastic differential equations", J. Time Series Analysis 24(1), January 2003.
% [2] D.W. Scott "Multivariate density estimation", Wiley & Sons, 1992.

% <NEXT> We have to consider the general case (see [2] section 6.3.2).

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
if(strcmp(upper(INTEGRATOR),'EM')==0 && strcmp(upper(INTEGRATOR),'MIL')==0)
    error('INTEGRATOR must be ''EM'' (Euler-Maruyama) or ''MIL'' (Milstein)');
end
if(strcmp(upper(SDETYPE),'ITO')==0 && strcmp(upper(SDETYPE),'STRAT')==0)
    error('SDETYPE must be ''ITO'' or ''STRAT'' (Stratonovich)');
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
nvar = length(unique(VRBL));
xhat_endpoint = zeros(n-1,nvar*NUMSIM);

switch upper(INTEGRATOR) 
case 'EM'  % the Euler-Maruyama integration scheme
      xhat_endpoint = SDE_NPSML_euler(bigtheta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,NUMDEPVARS,SEED);
case 'MIL'  % the Milstein integration scheme
      xhat_endpoint = SDE_NPSML_milstein(bigtheta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,NUMDEPVARS,SEED);
end

% now we want only consider the last n-1 values of XOBS because the first value(s) of XOBS is the deterministic initial condition(s) 
density = zeros(n-1,NUMSIM);
kernestim = zeros(n-1,NUMSIM*nvar);
kernarg = zeros(n-1,NUMSIM*nvar);       
bandwidth = zeros(n-1,nvar); 
xobsmatrix = repmat(XOBS(2:end,:),1,NUMSIM) ;

% the bandwidth
for(current_var = 1:nvar)
   bandwidth(:,current_var) = (4 / (nvar+2))^(1/(nvar + 4)) * std(xhat_endpoint(:,current_var:nvar:end),1,2) * NUMSIM^(-1/(nvar+4)); % see [2] section 6.3.1
end
bandwidthrep = repmat(bandwidth,1,NUMSIM);

kernarg = (xobsmatrix - xhat_endpoint) ./ bandwidthrep;   % the argument of the normal kernel
kernestim = exp(-0.5 * kernarg .^2) ./ (sqrt(2*pi));      % the normal kernel

if(nvar==1)
   density = kernestim;  
else  % vectorized code (for ease of clearness the non-vectorized code is reported below)
   B = reshape(kernestim',NUMDEPVARS,[])';  % ugly but necessary
   density = prod(B,2); % assume independence between variables
   density = reshape(density',NUMSIM,[])'; % return to the original dimensions of kernesim
% here follows the non-vectorized version   
%   columns = 1:nvar;
%   for(i=1:NUMSIM)
%       density(:,i) = prod(kernestim(:,columns),2);  % assume independence between variables
%       columns = columns + nvar;
%   end
end

transdensity = sum(density,2) ./ (NUMSIM * prod(bandwidth,2)); % the approximated transition densities
negloglikelihood = - sum(log(transdensity));  % the negative loglikelihood
