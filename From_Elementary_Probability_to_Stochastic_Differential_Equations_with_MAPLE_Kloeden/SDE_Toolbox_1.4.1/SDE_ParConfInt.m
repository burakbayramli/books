function [lowconf,upconf] = SDE_ParConfInt(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED)

% Display the estimated and constant parameters and computes the asymptotic 95% confidence intervals for the approximated maximum 
% likelihood estimates of the parameters using central approximation of Hessian.
%
% usage: [lowconf,upconf] = SDE_ParConfInt(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED)
%
% IN:
% lossfunction; the name of the function computing the NEGATIVE log-likelihood at theta; can be 'SDE_PSML' or 'SDE_NPSML'
%        theta; the FREE parameter values at which to compute the hessian
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
% OUT: lowconf; the array containing the lower bound of the 95% confidence limits for the estimated parameters
%       upconf; the array containing the upper bound of the 95% confidence limits for the estimated parameters
%
% See also SDE_PSML and SDE_NPSML

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
error(nargchk(16, 16, nargin));
[n1,n2]=rat(NUMSIM);
if(~isequal(n2,1) || isempty(NUMSIM) || NUMSIM <= 0)
    error('The number of trajectories NUMSIM must be a positive integer');
end
[n1,n2]=rat(NUMDEPVARS);
if(~isequal(n2,1) || isempty(NUMDEPVARS) || NUMDEPVARS <= 0)
    error('The number of variables NUMDEPVARS must be a positive integer');
end
if(strcmp(upper(SDETYPE),'ITO')==0 && strcmp(upper(SDETYPE),'STRAT')==0)
    error('SDETYPE must be ''ITO'' or ''STRAT'' (Stratonovich)');
end
if(strcmp(upper(INTEGRATOR),'EM')==0 && strcmp(upper(INTEGRATOR),'MIL')==0)
    error('INTEGRATOR must be ''EM'' (Euler-Maruyama) or ''MIL'' (Milstein)');
end
if(strcmp(upper(lossfunction),'SDE_PSML')==0 && strcmp(upper(lossfunction),'SDE_NPSML')==0)
    error('LOSSFUNCTION must be ''SDE_PSML'' or ''SDE_NPSML''');
end

% approximate the Hessian of the negative loglikelihood specified in
% 'lossfunction' ('SDE_compute_hessian' changes the sign automatically to return the hessian of the
% loglikelihood)
hessian =  SDE_compute_hessian(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);  % the Hessian of the log-likelihood function at theta
% change the sign another time to obtain the observed Fisher information
information = - hessian;  
variance_theta = diag(inv(information));  % the asymptotic variances for the free parameters

lowconf = zeros(1,length(theta));
upconf = zeros(1,length(theta));

fprintf('\n\n\nESTIMATED PARAMETER VALUES AND 95 pct. CONFIDENCE INTERVALS\n');
fprintf('------------------------------------------------------');
for p=1:length(theta)
    fprintf('\nfree parameter #%d):\t%1d\t[%10.5g, %10.5g]',p,theta(p),theta(p)-1.96*sqrt(variance_theta(p)),theta(p)+1.96*sqrt(variance_theta(p)));
    lowconf(p) = theta(p)-1.96*sqrt(variance_theta(p));
    upconf(p) = theta(p)+1.96*sqrt(variance_theta(p));
end

bigtheta =  SDE_param_unmask(theta,PARMASK,PARBASE); % the full set of estimated and constant parameters
fprintf('\n\n\nCONSTANT PARAMETER VALUES\n');
fprintf('------------------------------------------------------');
for p=1:length(bigtheta)
   if(PARMASK(p)==0)
      fprintf('\nconstant parameter #%d):\t%1d',p,bigtheta(p));
   end
end