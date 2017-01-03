function hessian = SDE_compute_hessian(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED) 

% Computes central approximation of Hessian at theta for free parameters with check on parameter bounds.  
%
% usage: hessian = SDE_compute_hessian(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED) 
%
% IN:    
% lossfunction; the name of the function computing the NEGATIVE log-likelihood at theta; can be 'SDE_PSML' or 'SDE_NPSML'
%        theta; the FREE parameter values at which to compute the hessian
%      OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%               It has starting simulation-time in first and ending simulation-time in last position. 
%               Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize for the numerical integration (i=2,3,...)
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
% OUT: hessian; the central Hessian matrix for the log-likelihood at theta

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

hessian_target_step = 1.e-5;
npar = length(theta);

% Compute baseline loss
fatmin = feval(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);   

if (SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE) > 0)
    error('\n\n\nUNACCEPTABLE STARTING VALUES FOR HESSIAN COMPUTATION');
end


% Create and initialize to Identity
hessian = eye(npar);

% fprintf(1,'\n\nComputing Hessian:');
handle = waitbar(0,'Computing Hessian...');

for p=1:npar
%     fprintf(1,'.');
    waitbar(p/npar);
    
    pvalue = theta(p);
    
    % DIAGONAL ELEMENT
    pstep = hessian_target_step * (1 + abs(pvalue));
    
    theta(p) = pvalue - pstep; % check on the low side...
    while (SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE) > 0)
        pstep = pstep / 2.;
        theta(p) = pvalue - pstep;
    end
    
    theta(p) = pvalue + pstep;  % and continue checking on the high side...
    while (SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE) > 0)
        pstep = pstep / 2.;
        theta(p) = pvalue + pstep;
    end
    
    fpp = feval(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);  
    theta(p) = pvalue - pstep;
    fpm = feval(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);  
    
    hessian(p,p)= (fpp + fpm - 2. * fatmin) / (pstep * pstep) ;
    
    % OFF-DIAGONAL ELEMENTS
    for q=p+1:npar
        qvalue = theta(q);
        pstep = hessian_target_step * (1 + abs(pvalue));
        qstep = hessian_target_step * (1 + abs(qvalue));
        
        failureflag=0;
        theta(p) = pvalue - pstep;
        theta(q) = qvalue - qstep;
        failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
        theta(p) = pvalue - pstep;
        theta(q) = qvalue + qstep;
        failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
        theta(p) = pvalue + pstep;
        theta(q) = qvalue - qstep;
        failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
        theta(p) = pvalue + pstep;
        theta(q) = qvalue + qstep;
        failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
        while (failureflag > 0)
            pstep = pstep/2;
            qstep = qstep/2;
            failureflag=0;
            theta(p) = pvalue - pstep;
            theta(q) = qvalue - qstep;
            failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
            theta(p) = pvalue - pstep;
            theta(q) = qvalue + qstep;
            failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
            theta(p) = pvalue + pstep;
            theta(q) = qvalue - qstep;
            failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
            theta(p) = pvalue + pstep;
            theta(q) = qvalue + qstep;
            failureflag = failureflag + SDE_parcheck(theta,PARMIN,PARMAX,PARMASK,PARBASE);
        end
        
        theta(p) = pvalue - pstep;
        theta(q) = qvalue - qstep;
        fpmqm = feval(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);  
        
        theta(p) = pvalue - pstep;
        theta(q) = qvalue + qstep;
        fpmqp = feval(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);  
        
        theta(p) = pvalue + pstep;
        theta(q) = qvalue - qstep;
        fppqm = feval(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);  
        
        theta(p) = pvalue + pstep;
        theta(q) = qvalue + qstep;
        fppqp = feval(lossfunction,theta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,PARBASE,PARMIN,PARMAX,PARMASK,INTEGRATOR,NUMDEPVARS,SEED);  
        
        hessian(p,q) = (fppqp - fppqm - fpmqp + fpmqm) / (4 * pstep * qstep) ;
        
        theta(q) = qvalue;
    end
    theta(p) = pvalue;
end

close(handle);

% retrieve other half of the hessian 
for p=1:npar
	for q=1:p-1
		hessian(p,q)= hessian(q,p);
	end
end

% return the correct hessian: in fact the 'lossfunction' used in the feval invocations above is the NEGATIVE
% loglikelihood
hessian = - hessian;


