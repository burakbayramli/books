function xhat_endpoint = SDE_NPSML_milstein(bigtheta,OWNTIME,TIME,VRBL,XOBS,PROBLEM,NUMSIM,SDETYPE,NUMDEPVARS,SEED)

% Returns the Milstein (read the warning below) xhat estimated values at n time-points t_1,t_2,...,t_n for a single trajectory 
% of the stochastic model dX(t) =... defined by a system of (Ito or Stratonovich) SDEs considered on the time-intervals [t_0,t_1],...,[t_(n-1),t_n] where, 
% for each time-interval, the initial condition is given by X(t_(k)) = XOBS(t_(k)),    k=1,...,n-1.
%
% Warning: To be used ONLY with the implementation of the Non-Parametric SML algorithm as described in [1] (see SDE_NPSML).
%          For different purposes use SDE_milstein.m
%
% IN:
%     bigtheta; complete structural parameter vector
%      OWNTIME; vector containing the equispaced simulation times sorted in ascending order. 
%               It has starting simulation-time in first and ending simulation-time in last position. 
%               Thus OWNTIME(i) - OWNTIME(i-1) = h, where h is the fixed stepsize 
%               for the numerical intregration (i=2,3,...)
%         TIME; the array of unique observation times
%         VRBL; the array of unique label-variables 
%         XOBS; the matrix-shaped observed data
%       NUMSIM; the number of desired simulations for the SDE numerical integration 
%      PROBLEM; the user defined name of the current problem/experiment/example etc. (e.g. 'mySDE')
%      SDETYPE; the SDE definition: can be 'Ito' or 'Strat' (Stratonovich)
%   NUMDEPVARS; the number of dependent variables, i.e. the SDE dimension
%         SEED; the seed for the generation of pseudo-random normal variates, i.e. the argument for randn('state',SEED);
%               type 'help randn' for details;
% OUT: xhat_endpoint; the Euler-Maruyama approximation at the intervals end-points
%
% References:
% [1] A.S. Hurn, K.A. Lindsay and V.L. Martin "On the effficacy of simulated maximum likelihood for estimating the parameters of 
%     stochastic differential equations", J. Time Series Analysis vol. 24, n° 1 january 2003.
% [2] Kloeden and Platen "Numerical solution of Stochastic Differential Equation", Springer-Verlag 1991.

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
if(strcmp(upper(SDETYPE),'ITO')==0 && strcmp(upper(SDETYPE),'STRAT')==0)
    error('SDETYPE must be ''ITO'' or ''STRAT'' (Stratonovich)');
end

n = length(unique(TIME));
h = OWNTIME(2) - OWNTIME(1);
if(h > min(diff(TIME(diff(TIME)>0))))
   error('Choose a smaller stepsize')
end
nvar = length(unique(VRBL));
xhat_endpoint = zeros(n-1,NUMSIM*nvar);
time = unique(TIME);


% by controlling the seed, the Wiener increments are kept fixed during a given optimization procedure         
if(isempty(SEED))
    randn('state',0);
else
    randn('state',SEED);
end 


for j=1:n-1 
    
    if(j==1)
        t = time(j);
        [t, xstart] = feval([PROBLEM, '_sdefile'], t, [], 'init', bigtheta,SDETYPE,NUMDEPVARS,NUMSIM);
        xstart = XOBS(j,:); % fills xstart with the appropriate observed values  
        discretization = [time(j):h:time(j+1)];
        d = length(discretization);
        if(d<3)
           error('Choose a smaller integration stespize');
        end
        XVARS = zeros(d,nvar*NUMSIM);  
        xstart = xstart';
        XVARS(1,:) = xstart([1:size(xstart,1)]' * ones(1,NUMSIM), :)'; % ugly but faster than XVARS(1,:) = repmat(xstart,1,NUMSIM);
    else
        xobs = XOBS(j,:);
        discretization = [time(j):h:time(j+1)];
        d = length(discretization);
        if(d<3)
           error('Choose a smaller integration stespize');
        end
        XVARS = zeros(d,nvar*NUMSIM);
        xobs = xobs';
        XVARS(1,:) = xobs([1:size(xobs,1)]' * ones(1,NUMSIM), :)'; % fills the XVARS(1,:) columns with the appropriate observed values  
        t = time(j);
    end

    for i=(2:d)
        x = XVARS(i-1, :); 
        
        % Wiener increments generation with 'antithetic variates' variance reduction method
        Winc = zeros(1,nvar*NUMSIM);
        if(mod(nvar*NUMSIM,2)==0)
           Winc(1 : nvar * NUMSIM/2) = sqrt(h)*randn(1,nvar * NUMSIM/2); % the Wiener increment(s) dWj; 
           Winc(nvar * NUMSIM/2 + 1 : end) = -Winc(1 : nvar * NUMSIM/2); % retrieve the other half of the increments using the 'antithetic variates' variance reduction method;
        else
           % adjustment when (number of simulations * number of variables) is odd
           Winc(1 : round(NUMSIM*nvar/2)) = sqrt(h)*randn(1,round(NUMSIM*nvar/2));  % the Wiener increment(s) dWj; 
           Winc(round(NUMSIM*nvar/2)+1:end) = -Winc(1 : round(NUMSIM*nvar/2)-1);  % retrieve the other half of the increments using the 'antithetic variates' variance reduction method;
        end
        
        [f,g,dg] = feval([PROBLEM, '_sdefile'], t, x, [], bigtheta,SDETYPE,NUMDEPVARS,NUMSIM);     % the sdefile output
       
        switch upper(SDETYPE)
        case 'ITO'
              XVARS(i , :) = x + f * h + g .* Winc + 1/2 * g .* dg .* (Winc.^2-h) ;  % the Milstein scheme for Ito SDEs with 'diagonal noise' 
        case 'STRAT'
              XVARS(i , :) = x + f * h + g .* Winc + 1/2 * g .* dg .* Winc.^2 ;  % the Milstein scheme for Stratonovich SDEs with 'diagonal noise'      
        end
       
        t = discretization(i) ;   % now both t and i refer to the end-of-interval    
    end  

    xhat_endpoint(j,:) = XVARS(end ,:);  
end
        
